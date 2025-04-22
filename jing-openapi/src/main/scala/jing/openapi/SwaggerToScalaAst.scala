package jing.openapi

import io.swagger.parser.OpenAPIParser
import jing.openapi.Mode.IsSubsumedBy
import jing.openapi.Mode.IsSubsumedBy.given
import jing.openapi.ModelToScalaAst.{*, given}
import jing.openapi.StructuralRefinement.{MemberDef, PreviousSiblings, typeRefUnsafe}
import jing.openapi.model.{
  ||,
  ::,
  :?,
  BodySchema,
  DiscriminatedUnion,
  HttpEndpoint,
  HttpMethod,
  Obj,
  OpenApiSpec,
  RequestSchema,
  ResponseSchema,
  Schema,
  SchemaCompanion,
  SchemaMotif,
  Str,
  Value,
}
import libretto.lambda.Items1Named
import libretto.lambda.util.{Applicative, Exists, Functor, SingletonType, TypeEq, TypeEqK}
import libretto.lambda.util.Applicative.pure
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq.Refl
import scala.collection.immutable.{:: as NonEmptyList}
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.annotation.tailrec

private[openapi] object SwaggerToScalaAst {
  def apply(location: String)(using q: Quotes): Expr[Any] = {
    import quotes.reflect.*

    val spec = new OpenAPIParser().readLocation(location, null, null).getOpenAPI()

    val schemas0: List[(String, ProtoSchema)] = {
      val b = List.newBuilder[(String, ProtoSchema)]
      spec.getComponents().getSchemas().forEach { (name, schema) => b += ((name, protoSchema(schema))) }
      b.result()
    }

    val schemas: List[(String, ProtoSchema.Oriented)] =
      ProtoSchema.Oriented.sortTopologically(schemas0)

    val paths: List[(String, io.swagger.v3.oas.models.PathItem)] = {
      val b = List.newBuilder[(String, io.swagger.v3.oas.models.PathItem)]
      spec.getPaths().forEach { (name, path) => b += ((name, path)) }
      b.result()
    }

    StructuralRefinement.typedTerm[OpenApiSpec](
      owner = Symbol.spliceOwner,
      members = List(
        "schemas" -> schemasField["term-synth"](schemas),
        "paths"   -> pathsField["term-synth"](schemas, paths),
      ),
      "Api",
    ).asExpr
  }

  private def schemasField[M](using q: Quotes)(
    schemas: List[(String, ProtoSchema.Oriented)],
  ): MemberDef.Poly[q.type, M] = {
    import q.reflect.*

    MemberDef.poly[q.type, M] { [N] => (_, _) ?=> (ctx: PreviousSiblings[q.type, N]) =>
      def resolveSchema[M](using mode: Mode[q.type, M])(
        ctx: PreviousSiblings[q.type, M],
        s: ProtoSchema.Oriented,
      ): Exists[[T] =>> (Type[T], mode.OutEff[Expr[Schema[T]]])] =
        quotedSchemaFromProto[mode.OutEff](s)
          .run(schemaLookupFromPreviousSiblings(ctx))

      val (tpe, bodyFn) = StructuralRefinement.forMode[N][AnyRef](
        members = schemas.flatMap { case (name, s) =>
          List(
            // "opaque" type alias
            name -> MemberDef.poly[q.type, N] { [N] => (_, _) ?=> ctx =>
              val tpe = resolveSchema[N](ctx, s).value._1
              MemberDef.Type(TypeRepr.of(using tpe))
            },

            // companion "object"
            name -> MemberDef.poly[q.type, N] { [N] => (n, _) ?=> ctx =>
              val ex: Exists[[T] =>> (Type[T], n.OutEff[Expr[Schema[T]]])] = resolveSchema[N](ctx, s)
              given Type[ex.T] = ex.value._1
              val tpeAlias =
                ctx.types
                  .getOrElse(name, { throw AssertionError(s"Type `$name` not found, even though it should have just been defined") })
                  .asType
                  .asInstanceOf[Type[? <: Any]]
              val (tp, bodyFn) = schemaCompanion(using q, tpeAlias, summon[Type[ex.T]])(name, ex.value._2)
              MemberDef.Val(tp, bodyFn)
            },
          )
        },
        "schemas",
      )
      MemberDef.Val(tpe, bodyFn)
    }
  }

  private def pathsField[M](using q: Quotes)(
    schemas: List[(String, ProtoSchema.Oriented)],
    paths: List[(String, io.swagger.v3.oas.models.PathItem)],
  ): MemberDef.Poly[q.type, M] = {
    import quotes.reflect.*

    MemberDef.poly[q.type, M] { [M1] => (m1, _) ?=> ctx =>
      val schemasField: ctx.mode.InTerm =
        ctx.terms.getOrElse("schemas", { throw AssertionError("field `schemas` not previously defined") })

      val schemaLookup: SchemaLookup[m1.OutEff] =
        schemaLookupFromSchemaField[M1](Mode.sameInTerm(ctx.mode, m1)(schemasField), schemas.map(_._1))

      val (tpe, bodyFn) = StructuralRefinement.forMode[M1][AnyRef](
        members = paths.map { case (path, pathItem) =>
          path -> MemberDef.poly[q.type, M1] { [M2] => (m2, sub) ?=> _ =>
            val (tpe, bodyFn) = pathToObject[M2](schemaLookup.mapK(sub.downgrader), path, pathItem)
            MemberDef.Val(tpe, bodyFn)
          }
        },
        "paths",
      )
      MemberDef.Val(tpe, bodyFn)
    }
  }

  /** For each previous term `Foo`, assumes it is a companion of a previous type `Foo`,
   *  and selects `Foo.schema` as a term of type `Schema[Foo]`.
   */
  def schemaLookupFromPreviousSiblings[M](using q: Quotes, mode: Mode[q.type, M])(
    ctx: PreviousSiblings[q.type, M],
  ): SchemaLookup[mode.OutEff] = {
    import q.reflect.*

    val schemaLookup0: SchemaLookup[ctx.mode.OutEff] =
      SchemaLookup.fromMap[ctx.mode.OutEff](
        ctx.terms.transform: (name, companionTerm) =>
          val tpe = ctx.types(name)
          val schemaTerm: ctx.mode.OutEff[qr.Term] =
            ctx.mode.term(companionTerm)
              .map[qr.Term](Select.unique(_, "schema"))
          typeAndSchemaExpr(tpe, schemaTerm)
      )

    Mode.sameOutEff(ctx.mode, mode)
      .subst(schemaLookup0)
  }

  /** Collects schema types (like `schemas.Foo`) and schema terms (like `schemas.Foo.schema`)
   *  from the given `schemas` field, for use within the same parent as that of the `schemas` field.
   */
  private def schemaLookupFromSchemaField[M](using q: Quotes, m: Mode[q.type, M])(
    schemasField: m.InTerm,
    schemaNames: List[String],
  ): SchemaLookup[m.OutEff] = {
    import q.reflect.*

    val typesAndTerms: Map[String, Exists[[T] =>> (Type[T], m.OutEff[Expr[Schema[T]]])]] =
      m match {
        case _: Mode.TermSynth[q] =>
          val schemasFieldTerm: Term =
            m.inTermProper(schemasField)
          schemaNames
            .map[(String, Exists[[T] =>> (Type[T], m.OutEff[Expr[Schema[T]]])])] { name =>
              val tpe: TypeRepr =
                TypeSelect(schemasFieldTerm, name).tpe

              val companionDynamic =
                Select.unique(schemasFieldTerm, "selectDynamic")
                  .appliedTo(Literal(StringConstant(name)))
              val companionTyped =
                Select
                  .unique(companionDynamic, "$asInstanceOf$")
                  .appliedToType(
                    TypeRepr.of[SchemaCompanion]
                      .appliedTo(List(
                        TypeSelect(schemasFieldTerm, name).tpe,
                        WildcardTypeTree(TypeBounds.empty).tpe)
                      ),
                  )
              val schemaTerm =
                Select.unique(companionTyped, "schema")

              (name, typeAndSchemaExpr(tpe, schemaTerm.pure[m.OutEff]))
            }
            .toMap

        case _: Mode.TypeSynth[q] =>
          val schemasFieldRef: TermRef =
            m.inTermRefOnly(schemasField)
          val fabricate: [A] => Unit => m.OutEff[A] =
            val ev = m.outEffConstUnit.flip;
            [A] => (u: Unit) => ev.at[A](())
          schemaNames
            .map[(String, Exists[[T] =>> (Type[T], m.OutEff[Expr[Schema[T]]])])] { name =>
              val tpe = typeRefUnsafe(schemasFieldRef, name)
              (name, typeAndSchemaExpr(tpe, fabricate(())))
            }
            .toMap
      }

    SchemaLookup.fromMap[m.OutEff](typesAndTerms)
  }

  private def typeAndSchemaExpr[F[_]](using Quotes)(
    tpe: qr.TypeRepr,
    schemaTerm: F[qr.Term],
  )(using Functor[F]): Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])] = {
    tpe.asType match
      case '[t] => Indeed((Type.of[t], schemaTerm.map(_.asExprOf[Schema[t]])))
  }

  /**
    * @tparam A the abstract ("opaque") alias (of `T`) for which we are creating the companion object
    * @tparam T the definition of `A`
    * @tparam M mode (`"term-synth"` or `"type-synth"`)
    */
  private def schemaCompanion[A, T, M](using q: Quotes, ta: Type[A], tt: Type[T], m: Mode[q.type, M])(
    name: String,
    schema: m.OutEff[Expr[Schema[T]]],
  ): (qr.TypeRepr, m.OutEff[(owner: qr.Symbol) => qr.Term]) = {
    import qr.*

    StructuralRefinement.forMode[M][SchemaCompanion[A, T]](
      members = List(
        // schema of the "opaque" type
        "schema" -> MemberDef.poly[q.type, M] { [N] => (_, sub) ?=> ctx =>
          MemberDef.Val(TypeRepr.of[Schema[A]], sub.downgrader(schema.map { expr => _ => expr.asTerm }))
        },

        // constructor of the "opaque" type
        "apply" -> MemberDef.poly[q.type, M] { [N] => (n, _) ?=> _ =>
          MemberDef.Method(
            MethodType(paramNames = List("x"))(
              self => List(TypeRepr.of[Value[T]]),
              self => TypeRepr.of[Value[A]],
            ),
            body = n.applicativeOutEff.pure { (self, argss) =>
              // implementation is just identity
              val List(List(x)) = argss
              x.asExpr.asTerm
            },
          )
        },

        // deconstructor, overrides unapply from TotalExtractor
        "unapply" -> MemberDef.poly[q.type, M] { [N] => (n, _) ?=> _ =>
          MemberDef.Method(
            MethodType(paramNames = List("x"))(
              self => List(TypeRepr.of[Value[A]]),
              self => TypeRepr.of[Some[Value[T]]],
            ),
            body = n.applicativeOutEff.pure { (self, argss) =>
              // implementation is just wrapping the argument in Some
              val List(List(x)) = argss
              '{ Some(${x.asExprOf[Value[T]]}) }.asTerm
            },
          )
        },
      ),
      name,
    )
  }

  private def pathToObject[M](using q: Quotes, mode: Mode[q.type, M])(
    schemas: SchemaLookup[mode.OutEff],
    path: String,
    pathItem: io.swagger.v3.oas.models.PathItem,
  ): (qr.TypeRepr, mode.OutEff[(owner: qr.Symbol) => qr.Term]) = {
    import quotes.reflect.*

    val operations =
      HttpMethod.values.toList
        .flatMap { m => pathOperation(pathItem, m).map((m, _)) }

    StructuralRefinement.forMode[M][AnyRef](
      members = operations.map { case (meth, op) =>
        meth.toString -> MemberDef.poly[q.type, M] { [N] => (_, sub) ?=> _ =>
          operationToObject(schemas.mapK(sub.downgrader), path, meth, op) match
            case Indeed((tp, body)) =>
              MemberDef.Val(TypeRepr.of(using tp), body.map { b => (_: Symbol) => b.asTerm })
        }
      },
      path,
    )
  }

  private def pathOperation(
    path: io.swagger.v3.oas.models.PathItem,
    method: HttpMethod,
  ): Option[io.swagger.v3.oas.models.Operation] =
    Option(method match
        case HttpMethod.Get     => path.getGet()
        case HttpMethod.Post    => path.getPost()
        case HttpMethod.Put     => path.getPut()
        case HttpMethod.Delete  => path.getDelete()
        case HttpMethod.Head    => path.getHead()
        case HttpMethod.Options => path.getOptions()
        case HttpMethod.Patch   => path.getPatch()
        case HttpMethod.Trace   => path.getTrace()
    )

  private type QuotedSchema[F[_]] =
    Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])]

  private enum ProtoParam[F[_]]:
    case PathParam(name: String, schema: QuotedSchema[F])
    case QueryParam(name: String, schema: QuotedSchema[F], required: Boolean)
    case Unsupported(name: String, schema: QuotedSchema[F], required: Boolean, unsupportedLocation: String)

  private def resolveParam[F[_]](
    schemas: SchemaLookup[F],
    param: io.swagger.v3.oas.models.parameters.Parameter,
  )(using
    q: Quotes,
    F: Applicative[F],
  ): ProtoParam[F[_]] = {
    val name = param.getName
    val schema =
      quotedSchemaFromProto(
        protoSchema(param.getSchema()).orientBackward,
      ).run(schemas)

    val required: Boolean =
      param.getRequired() match
        case null => false
        case other => other

    param.getIn match
      case "path"   => ProtoParam.PathParam(name, schema)
      case "query"  => ProtoParam.QueryParam(name, schema, required)
      case other    => ProtoParam.Unsupported(name, schema, required, other)
  }


  private def operationToObject[F[_]](
    schemas: SchemaLookup[F],
    path: String,
    method: HttpMethod,
    op: io.swagger.v3.oas.models.Operation,
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[T]])] = {
    import qr.*

    val paramSchema: Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.ParamsOpt[Ps]]])] =
      requestSchemaParamsOpt(
        path,
        Option(op.getParameters())
          .map(_.asScala.toList)
          .getOrElse(Nil)
          .map(resolveParam(schemas, _))
      )

    val reqBodySchema: Option[Exists[[B] =>> (Type[B], F[Expr[BodySchema.NonEmpty[B]]])]] =
      Option(op.getRequestBody())
        .flatMap(requestBodySchema(schemas, _))

    val reqSchema: Exists[[T] =>> (Type[T], F[Expr[RequestSchema[T]]])] =
      requestSchema(paramSchema, reqBodySchema)

    import scala.collection.immutable.::
    val responseSchema =
      Option(op.getResponses())
        .map(_.entrySet().iterator().asScala.map(e => (e.getKey(), e.getValue())).toList)
        .collect { case r :: rs => NonEmptyList(r, rs) }
        .map(responseBodyByStatus(schemas, _))
        .getOrElse {
          report.errorAndAbort(s"No response defined for $method $path")
        }

    (reqSchema, responseSchema) match
      case (req @ Indeed((tReq, reqSchema)), resp @ Indeed((tResp, respSchema))) =>
        type I = req.T
        type O = resp.T
        given Type[I] = tReq
        given Type[O] = tResp

        Indeed((
          Type.of[HttpEndpoint[I, O]],
          F.map2(reqSchema, respSchema) { (reqSchema, respSchema) =>
            '{ HttpEndpoint(${Expr(method)}, $reqSchema, $respSchema) }
          },
        ))
  }

  private def requestSchemaParamsOpt[F[_]](
    path: String,
    params: List[ProtoParam[F]],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.ParamsOpt[Ps]]])] =
    parametersSchema(path, params) match
      case Some(ex @ Indeed((t, e))) =>
        given Type[ex.T] = t
        Indeed((
          Type.of[Void || "params" :: Obj[ex.T]],
          e.map { e => '{ RequestSchema.Parameterized($e) } }
        ))
      case None =>
        Indeed((Type.of[Void], F.pure('{ RequestSchema.ConstantPath(${Expr(path)}) })))

  private def parametersSchema[F[_]](
    path: String,
    params: List[ProtoParam[F]],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Option[Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.Params.Proper[Ps]]])]] = {
    val (pathParams, params1) =
      params.partitionMap:
        case p: ProtoParam.PathParam[F] => Left(p)
        case p: (ProtoParam.QueryParam[F] | ProtoParam.Unsupported[F]) => Right(p)

    val (queryParams, params2) =
      params1.partitionMap:
        case p: ProtoParam.QueryParam[F] => Left(p)
        case p: ProtoParam.Unsupported[F] => Right(p)

    // TODO: support header params

    val unsupportedAsQueryParams: List[ProtoParam.QueryParam[F]] =
      params2.map {
        case ProtoParam.Unsupported(name, _, required, unsupportedLocation) =>
          val schema = Schema.unsupported(s"'$unsupportedLocation' parameters are not yet supported")
          val (tp, exp) = quotedSchema(schema)
          ProtoParam.QueryParam(name, Indeed((tp, F.pure(exp))), required)
        }

    val queryParams1 = queryParams ++ unsupportedAsQueryParams

    pathSchema(path, pathParams) match
      case Some(ex @ Indeed((t, fp))) =>
        given Type[ex.T] = t
        Some:
          appendQueryParams(
            Indeed((t, fp.map { p => '{ RequestSchema.Params.ParameterizedPath($p) } })),
            queryParams1,
          )
      case None =>
        queryParams1 match
          case NonEmptyList(p0, ps) =>
            Some:
              appendQueryParams(
                appendQueryParam(
                  '{ RequestSchema.Params.ConstantPath(${Expr(path)}) }.pure[F],
                  p0,
                ),
                ps
              )
          case Nil =>
            None
  }

  private def pathSchema[F[_]](
    path: String,
    pathParams: List[ProtoParam.PathParam[F]],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Option[Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.Path.Parameterized[Ps]]])]] = {
    // sort parameters by their occurence in path
    // and track their start, end, and start of the next parameter
    val sortedParams: List[(ProtoParam.PathParam[F], Int, Int, Int)] =
      pathParams
        .map: p =>
          val needle = "{" + p.name + "}"
          val i = path.indexOf(needle)
          (p, i, i+needle.length())
        .filter: // TODO: report error on params not found in path
          case (p, i, j) => i != -1
        .sortBy:
          case (p, i, j) => i
        .foldRight(Nil) { case ((p, i, j), acc) =>
          acc match
            case Nil =>
              (p, i, j, path.length) :: Nil
            case qs @ NonEmptyList((q, k, l, _), _) =>
              (p, i, j, k) :: qs
        }

    sortedParams match
      case Nil =>
        None
      case NonEmptyList((p0, i, j, k), ps) =>
        Some:
          ps.foldLeft(
            appendPathParam(
              '{ RequestSchema.Path.Constant(${Expr(path.substring(0, i))}) }.pure[F],
              p0,
              suffix = path.substring(j, k),
            )
          ) { case (acc @ Indeed((t, fe)), (p, i, j, k)) =>
            given Type[acc.T] = t
            appendPathParam(fe.widen, p, suffix = path.substring(j, k))
          }
  }

  private def appendPathParam[F[_], Init](
    init: F[Expr[RequestSchema.Path[Init]]],
    param: ProtoParam.PathParam[F],
    suffix: String,
  )(using
    q: Quotes,
    t: Type[Init],
    F: Applicative[F],
  ): Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.Path.Parameterized[Ps]]])] = {
    def go[PName <: String](
      pname: SingletonType[PName],
    ): Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.Path.Parameterized[Ps]]])] =
      val (nmt, nme) = ModelToScalaAst.quotedSingletonString(pname)
      given Type[PName] = nmt

      param.schema match
        case ex @ Indeed((t, fSch)) =>
          given Type[ex.T] = t
          Indeed((
            Type.of[Init || PName :: ex.T],
            F.map2(init, fSch) { (init, sch) => '{ RequestSchema.Path.WithParam($init, $nme, $sch, ${Expr(suffix)}) } },
          ))

    go[param.name.type](SingletonType(param.name))
  }

  private def appendQueryParams[F[_]](
    init: Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.Params.Proper[Ps]]])],
    queryParams: List[ProtoParam.QueryParam[F]],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.Params.Proper[Ps]]])] = {
    queryParams.foldLeft(init) { case (acc @ Indeed((t, fe)), p) =>
      given Type[acc.T] = t
      appendQueryParam(fe.widen, p)
    }
  }

  private def appendQueryParam[F[_], Init](
    init: F[Expr[RequestSchema.Params[Init]]],
    param: ProtoParam.QueryParam[F],
  )(using
    q: Quotes,
    t: Type[Init],
    F: Applicative[F],
  ): Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.Params.Proper[Ps]]])] = {
    def go[PName <: String](pname: SingletonType[PName]): Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.Params.Proper[Ps]]])] =
      val (nmt, nme) = ModelToScalaAst.quotedSingletonString(pname)
      given Type[PName] = nmt

      param.schema match
        case ex @ Indeed((t, fSch)) =>
          given Type[ex.T] = t
          if (param.required)
            Indeed((
              Type.of[Init || PName :: ex.T],
              F.map2(init, fSch) { (init, sch) => '{ RequestSchema.Params.WithQueryParam($init, $nme, $sch) } },
            ))
          else
            Indeed((
              Type.of[Init || PName :? ex.T],
              F.map2(init, fSch) { (init, sch) => '{ RequestSchema.Params.WithQueryParamOpt($init, $nme, $sch) } },
            ))

    go[param.name.type](SingletonType(param.name))
  }

  private def requestBodySchema[F[_]](
    schemas: SchemaLookup[F],
    requestBody: io.swagger.v3.oas.models.parameters.RequestBody,
  )(using
    Quotes,
    Applicative[F],
  ): Option[Exists[[T] =>> (Type[T], F[Expr[BodySchema.NonEmpty[T]]])]] =
    bodySchema(schemas, requestBody.getContent())

  private def bodySchema[F[_]](
    schemas: SchemaLookup[F],
    nullableContent: io.swagger.v3.oas.models.media.Content,
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Option[Exists[[T] =>> (Type[T], F[Expr[BodySchema.NonEmpty[T]]])]] =
    nonEmptyEntryList(nullableContent)
      .map { bodySchema(schemas, _) }

  def bodySchema[F[_]](
    schemas: SchemaLookup[F],
    mediaTypes: NonEmptyList[(String, io.swagger.v3.oas.models.media.MediaType)],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[BodySchema.NonEmpty[T]]])] =
    bodyVariants(schemas, mediaTypes) match
      case ex @ Indeed((t, fe)) =>
        given Type[ex.T] = t
        Indeed((
          Type.of[DiscriminatedUnion[ex.T]],
          fe map { e => '{ BodySchema.Variants($e) } }
        ))

  private def nonEmptyEntryList[K, V](nullableMap: java.util.Map[K, V]): Option[NonEmptyList[(K, V)]] =
    Option(nullableMap)
      .map(_.entrySet().iterator().asScala.map(e => (e.getKey(), e.getValue())).toList)
      .collect { case xs @ NonEmptyList(_, _) => xs }

  private def bodyVariants[F[_]](
    schemas: SchemaLookup[F],
    mediaTypes: NonEmptyList[(String, io.swagger.v3.oas.models.media.MediaType)],
  )(using
    Quotes,
    Applicative[F],
  ): Exists[[Ts] =>> (Type[Ts], F[Expr[Items1Named.Product[||, ::, Schema, Ts]]])] =
    quotedProductUnrelatedAA(
      mediaTypes
        .mapToProduct[[x] =>> ProtoSchema.Oriented](mt => Exists(protoSchema(mt.getSchema()).orientBackward)),
        [A] => ps => quotedSchemaFromProto[F](ps),
    ).run(schemas)

  private def responseBodyByStatus[F[_]](
    schemas: SchemaLookup[F],
    byStatus: NonEmptyList[(String, io.swagger.v3.oas.models.responses.ApiResponse)],
  )(using
    Quotes,
    Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[ResponseSchema[T]]])] =
    quotedProductUnrelatedAA(
      byStatus.asProduct,
      [A] => apiResponse => Reader((sl: SchemaLookup[F]) => responseBodySchemaOrPlainText(sl, apiResponse)),
    ).run(schemas) match {
      case x @ Indeed((tp, bs)) =>
        given Type[x.T] = tp
        Indeed((
          Type.of[DiscriminatedUnion[x.T]],
          bs.map { bs => '{ResponseSchema.ByStatusCode($bs) } }
        ))
    }

  private def responseBodySchemaOrPlainText[F[_]](
    schemas: SchemaLookup[F],
    apiResponse: io.swagger.v3.oas.models.responses.ApiResponse,
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[BodySchema[T]]])] =
    bodySchema(schemas, apiResponse.getContent()) match
      case Some(Indeed((tp, exp))) => Indeed((tp, exp.widen))
      case None => Indeed((Type.of[Str], F.pure('{ BodySchema.AnythingAsPlainText })))

  private def requestSchema[F[_]](
    paramsSchema: Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.ParamsOpt[Ps]]])],
    reqBodySchema: Option[Exists[[B] =>> (Type[B], F[Expr[BodySchema.NonEmpty[B]]])]],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[RequestSchema[T]]])] =
    paramsSchema match
      case ps @ Indeed((tps, sps)) =>
        given pst: Type[ps.T] = tps
        reqBodySchema match
          case Some(b @ Indeed((tb, sb))) =>
            given Type[b.T]  = tb
            Indeed((
              Type.of[ps.T || "body" :: b.T],
              F.map2(sps, sb) { (sps, sb) => '{ RequestSchema.WithBody($sps, $sb) } },
            ))
          case None =>
            Indeed((tps, sps.widen))

  private def protoSchema(using Quotes)(
    schema: io.swagger.v3.oas.models.media.Schema[?],
  ): ProtoSchema = {
    val LocalSchema = "#/components/schemas/(.*)".r
    schema.getType() match {
      case null =>
        schema.get$ref() match
          case null =>
            ProtoSchema.Unsupported("Swagger schema with no type or $ref")
          case LocalSchema(name) =>
            ProtoSchema.Ref(name)
          case ref =>
            ProtoSchema.Unsupported(s"The following $$ref format not yet supported: $ref")
      case "string" =>
        // TODO: look for modifiers such as format and enum
        ProtoSchema.str
      case "integer" =>
        schema.getFormat() match
          case "int32" => ProtoSchema.i32
          case "int64" => ProtoSchema.i64
          case other => ProtoSchema.Unsupported(s"Unsupported integer format: $other")
      case "boolean" =>
        ProtoSchema.bool
      case "array" =>
        val itemSchema = protoSchema(schema.getItems())
        ProtoSchema.arr(itemSchema)
      case "object" =>
        schema.getProperties() match
          case null =>
            ProtoSchema.Unsupported("Missing properties field in schema of type 'object'")
          case props =>
            val required: Set[String] =
              schema.getRequired() match
                case null => Set.empty[String]
                case props => (Set.newBuilder[String] ++= props.iterator.asScala).result()
            val b = List.newBuilder[(String, Boolean, ProtoSchema)]
            props.forEach { (name, s) => b += ((name, required.contains(name), protoSchema(s))) }
            ProtoSchema.obj(b.result())
      case other =>
        ProtoSchema.Unsupported(s"Type '$other' not yet supported.")
    }
  }

  extension [A](as: NonEmptyList[(String, A)]) {

    private def mapToProduct[F[_]](
      f: A => Exists[F],
    ): Items1Named.Product[||, ::, F, ?] = {

      @tailrec
      def go[Init](
        acc: Items1Named.Product[||, ::, F, Init],
        remaining: List[(String, A)],
      ): Items1Named.Product[||, ::, F, ?] =
        import scala.collection.immutable.::
        remaining match
          case Nil => acc
          case (tag, a) :: as =>
            f(a) match
              case Indeed(fa) =>
                go(
                  Items1Named.Product.Snoc(acc, SingletonType(tag), fa),
                  as,
                )

      val NonEmptyList((tag, a), tail) = as

      go(
        Items1Named.Product.Single(SingletonType(tag), f(a).value),
        tail,
      )
    }

    private def asProduct: Items1Named.Product[||, ::, [x] =>> A, ?] =
      mapToProduct[[x] =>> A](a => Indeed(a))

  }
}
