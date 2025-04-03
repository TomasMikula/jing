package jing.openapi

import io.swagger.parser.OpenAPIParser
import jing.openapi.Mode.IsSubsumedBy
import jing.openapi.Mode.IsSubsumedBy.given
import jing.openapi.ModelToScalaAst.{*, given}
import jing.openapi.model.{
  ||,
  ::,
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
  Schematic,
  Value,
}
import libretto.lambda.Items1Named
import libretto.lambda.util.{Applicative, Exists, SingletonType, TypeEq, TypeEqK}
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

    newRefinedObject[OpenApiSpec](
      owner = Symbol.spliceOwner,
      members = List(
        "schemas" -> MemberDef.poly[q.type, "term-synth"] { [M] => (_, _) ?=> (ctx: PreviousSiblings[q.type, M]) =>

          def resolveSchema[M](using mode: Mode[q.type, M])(
            ctx: PreviousSiblings[q.type, M],
            s: ProtoSchema.Oriented,
          ): Exists[[T] =>> (Type[T], mode.OutEff[Expr[Schema[T]]])] =
            quotedSchemaFromProto[mode.OutEff](s)
              .run(SchemaLookup.forMode[M](
                ctx.types,
                mode.isTermSynth.map { case TypeEq(Refl()) =>
                  ctx.termsProper.view.mapValues(Select.unique(_, "schema")).toMap
                },
              ))

          val (tpe, bodyFn) = newRefinedObject_[AnyRef, M](
            members = schemas.flatMap { case (name, s) =>
              List(
                // "opaque" type alias
                name -> MemberDef.poly[q.type, M] { [N] => (_, _) ?=> ctx =>
                  val tpe = resolveSchema[N](ctx, s).value._1
                  MemberDef.Type(TypeRepr.of(using tpe))
                },

                // companion "object"
                name -> MemberDef.poly[q.type, M] { [N] => (n, _) ?=> ctx =>
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
        },
        "paths" -> pathsField["term-synth"](schemas, paths),
      ),
      "Api",
    ).asExpr
  }

  private def pathsField[M](using q: Quotes)(
    schemas: List[(String, ProtoSchema.Oriented)],
    paths: List[(String, io.swagger.v3.oas.models.PathItem)],
  ): MemberDef.Poly[q.type, M] = {
    import quotes.reflect.*

    MemberDef.poly[q.type, M] { [M1] => (m1, _) ?=> ctx =>
      val schemasField: ctx.mode.InTerm =
        ctx.terms.getOrElse("schemas", { throw AssertionError("field `schemas` not previously defined") })

      val typesAndTerms: (Map[String, TypeRepr], m1.OutEff[Map[String, Term]]) =
        schemaRefsFromSchemaField[M1](Mode.sameInTerm(ctx.mode, m1)(schemasField), schemas.map(_._1))

      val schemaLookup: SchemaLookup[m1.OutEff] =
        SchemaLookup.forMode[M1](typesAndTerms._1, typesAndTerms._2)

      val (tpe, bodyFn) = newRefinedObject_[AnyRef, M1](
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

  /** Collects references to schema types (`schemas.Foo`) and schema terms (`schemas.Foo.schema`)
   *  from the given `schemas` field, for use within the parent of `schemas` field.
   */
  private def schemaRefsFromSchemaField[M](using q: Quotes, m: Mode[q.type, M])(
    schemasField: m.InTerm,
    schemaNames: List[String],
  ): (
    Map[String, qr.TypeRepr],
    m.OutEff[Map[String, qr.Term]],
  ) = {
    import q.reflect.*

    m match {
      case _: Mode.TermSynth[q] =>
        val schemasFieldTerm: Term =
          m.inTermProper(schemasField)
        val types: Map[String, TypeRepr] =
          schemaNames
            .map { name => (name, TypeSelect(schemasFieldTerm, name).tpe) }
            .toMap
        val terms: m.OutEff[Map[String, Term]] =
          schemaNames
            .map { name =>
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
              (name, schemaTerm)
            }
            .toMap
            .pure[m.OutEff]
        (types, terms)

      case _: Mode.TypeSynth[q] =>
        val schemasFieldRef: TermRef =
          m.inTermRefOnly(schemasField)
        (
          schemaNames
            .map { name => (name, typeRef(schemasFieldRef, name)) }
            .toMap,
          m.outEffConstUnit.flip.at[Map[String, Term]](()),
        )
    }
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

    newRefinedObject_[SchemaCompanion[A, T], M](
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

    newRefinedObject_[AnyRef, M](
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

    val paramSchema: Option[Exists[[Ps] =>> (Type[Ps], F[Expr[Schema[Obj[Ps]]]])]] =
      Option(op.getParameters())
        .map(_.asScala.toList)
        .collect { case p :: ps => NonEmptyList(p, ps) }
        .map(parametersSchema(schemas, _))

    val reqBodySchema: Option[Exists[[B] =>> (Type[B], F[Expr[BodySchema[B]]])]] =
      Option(op.getRequestBody())
        .map(requestBodySchema(schemas, _))

    val reqSchema: Exists[[T] =>> (Type[T], F[Expr[RequestSchema[T]]])] =
      requestSchema(paramSchema, reqBodySchema)

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
            '{ HttpEndpoint(${Expr(path)}, ${Expr(method)}, $reqSchema, $respSchema) }
          },
        ))
  }

  private def parametersSchema[F[_]](
    schemas: SchemaLookup[F],
    params: NonEmptyList[io.swagger.v3.oas.models.parameters.Parameter],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Exists[[Ps] =>> (Type[Ps], F[Expr[Schema[Obj[Ps]]]])] = {
    val protoSchematic: Schematic.Object[[x] =>> ProtoSchema.Oriented, ?] =
      params.foldLeft[Schematic.Object[[x] =>> ProtoSchema.Oriented, ?]](Schematic.Object.Empty()) { (acc, p) =>
        val pSchema = protoSchema(p.getSchema()).orientBackward
        Schematic.Object.snoc(acc, p.getName(), pSchema)
      }

    quotedObjectSchemaFromProto(protoSchematic)
      .run(schemas)
  }

  private def requestBodySchema[F[_]](
    schemas: SchemaLookup[F],
    requestBody: io.swagger.v3.oas.models.parameters.RequestBody,
  )(using
    Quotes,
    Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[BodySchema[T]]])] =
    bodySchema(schemas, requestBody.getContent())

  private def bodySchema[F[_]](
    schemas: SchemaLookup[F],
    nullableContent: io.swagger.v3.oas.models.media.Content,
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[BodySchema[T]]])] =
    bodyVariants(schemas, nullableContent) match
      case Some(ex @ Indeed((t, fe))) =>
        given Type[ex.T] = t
        Indeed((
          Type.of[DiscriminatedUnion[ex.T]],
          fe map { e => '{ BodySchema.Variants($e) } }
        ))
      case None =>
        Indeed((
          Type.of[Unit],
          F.pure('{ BodySchema.EmptyBody })
        ))

  private def bodyVariants[F[_]](
    schemas: SchemaLookup[F],
    nullableContent: io.swagger.v3.oas.models.media.Content,
  )(using
    Quotes,
    Applicative[F],
  ): Option[Exists[[Ts] =>> (Type[Ts], F[Expr[Items1Named.Product[||, ::, Schema, Ts]]])]] =
    Option(nullableContent)
      .map(_.entrySet().iterator().asScala.map(e => (e.getKey(), e.getValue())).toList)
      .collect { case x :: xs => NonEmptyList(x, xs) }
      .map { _.mapToProduct[[x] =>> ProtoSchema.Oriented](mt => Exists(protoSchema(mt.getSchema()).orientBackward)) }
      .map { xs =>
        quotedProductUnrelatedAA(
          xs,
          [A] => ps => quotedSchemaFromProto[F](ps),
        ).run(schemas)
      }

  private def responseBodyByStatus[F[_]](
    schemas: SchemaLookup[F],
    byStatus: NonEmptyList[(String, io.swagger.v3.oas.models.responses.ApiResponse)],
  )(using
    Quotes,
    Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[ResponseSchema[T]]])] =
    quotedProductUnrelatedAA(
      byStatus.asProduct,
      [A] => apiResponse => Reader((sl: SchemaLookup[F]) => responseBodySchema(sl, apiResponse)),
    ).run(schemas) match {
      case x @ Indeed((tp, bs)) =>
        given Type[x.T] = tp
        Indeed((tp, bs.map { bs => '{ResponseSchema($bs) } }))
    }

  private def responseBodySchema[F[_]](
    schemas: SchemaLookup[F],
    apiResponse: io.swagger.v3.oas.models.responses.ApiResponse,
  )(using
    Quotes,
    Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[BodySchema[T]]])] =
    bodySchema(schemas, apiResponse.getContent())

  private def requestSchema[F[_]](
    paramsSchema: Option[Exists[[Ps] =>> (Type[Ps], F[Expr[Schema[Obj[Ps]]]])]],
    reqBodySchema: Option[Exists[[B] =>> (Type[B], F[Expr[BodySchema[B]]])]],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[RequestSchema[T]]])] =
    (paramsSchema, reqBodySchema) match
      case (Some(ps @ Indeed((tps, sps))), Some(b @ Indeed((tb, sb)))) =>
        given pst: Type[ps.T] = tps
        given bt:  Type[b.T]  = tb
        Indeed((
          Type.of[Obj[{} || "params" :: Obj[ps.T] || "body" :: b.T]],
          F.map2(sps, sb) { (sps, sb) => '{ RequestSchema.ParamsAndBody($sps, $sb) } },
        ))
      case (Some(ps @ Indeed((tps, sps))), None) =>
        given Type[ps.T] = tps
        Indeed((
          Type.of[Obj[ps.T]],
          sps.map { sps => '{ RequestSchema.Params($sps) } },
        ))
      case (None, Some(b @ Indeed((tb, sb)))) =>
        given Type[b.T]  = tb
        Indeed((
          Type.of[b.T],
          sb.map { sb => '{ RequestSchema.Body($sb) } },
        ))
      case (None, None) =>
        Indeed((
          Type.of[Unit],
          F.pure( '{ RequestSchema.NoInput } )
        ))

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
        // TODO: support optionality of properties
        schema.getProperties() match
          case null =>
            ProtoSchema.Unsupported("Missing properties field in schema of type 'object'")
          case props =>
            val b = List.newBuilder[(String, ProtoSchema)]
            props.forEach { (name, s) => b += ((name, protoSchema(s))) }
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

  private class PreviousSiblings[Q <: Quotes, M](using
    val q: Q,
    val mode: Mode[q.type, M],
  )(
    val types: Map[String, qr.TypeRef],
    val terms: Map[String, mode.InTerm],
  ) {
    def addType(name: String, value: qr.TypeRef): PreviousSiblings[Q, M] =
      new PreviousSiblings(
        types.updated(name, value),
        terms,
      )

    def addTerm(using m: Mode[q.type, M])(name: String, value: m.InTerm): PreviousSiblings[Q, M] =
      val terms1 = Mode.sameInTerm(mode, m).substituteCo(terms)
      new PreviousSiblings[Q, M](
        types,
        terms1.updated(name, value),
      )

    def termsProper(using M =:= "term-synth"): Map[String, qr.Term] =
      mode.inTermProper.substituteCo(terms)
  }

  private object PreviousSiblings {
    def apply[M](using
      q: Quotes,
      mode: Mode[q.type, M],
    )(
      types: Map[String, qr.TypeRef],
      terms: Map[String, mode.InTerm],
    ): PreviousSiblings[q.type, M] =
      new PreviousSiblings[q.type, M](types, terms)

    def empty[M](using q: Quotes, m: Mode[q.type, M]): PreviousSiblings[q.type, M] =
      PreviousSiblings(Map.empty, Map.empty)
  }

  private sealed trait MemberDef[Q <: Quotes, +F[_]] {
    def acceptVisitor[R](
      caseType: (q: Q) ?=> qr.TypeRepr => R,
      caseVal: (q: Q) ?=> (tpe: qr.TypeRepr, body: F[(selfSym: qr.Symbol) => qr.Term]) => R,
      caseMethod: (q: Q) ?=> (tpe: qr.MethodType, body: F[(selfSym: qr.Symbol, argss: List[List[qr.Tree]]) => qr.Term]) => R,
    ): R
  }

  private object MemberDef {
    class Type[Q <: Quotes & Singleton](using val q: Q)(
      val body: qr.TypeRepr,
    ) extends MemberDef[Q, Nothing] {
      override def acceptVisitor[R](
        caseType: (q: Q) ?=> q.reflect.TypeRepr => R,
        caseVal: (q: Q) ?=> (tpe: q.reflect.TypeRepr, body: Nothing) => R,
        caseMethod: (q: Q) ?=> (tpe: q.reflect.MethodType, body: Nothing) => R,
      ): R =
        caseType(body)
    }

    class Val[Q <: Quotes & Singleton, F[_]](using val q: Q)(
      val tpe: qr.TypeRepr,
      val body: F[(selfSym: qr.Symbol) => qr.Term],
    ) extends MemberDef[Q, F] {
      override def acceptVisitor[R](
        caseType: (q: Q) ?=> q.reflect.TypeRepr => R,
        caseVal: (q: Q) ?=> (tpe: q.reflect.TypeRepr, body: F[(selfSym: q.reflect.Symbol) => q.reflect.Term]) => R,
        caseMethod: (q: Q) ?=> (tpe: q.reflect.MethodType, body: F[(selfSym: q.reflect.Symbol, argss: List[List[q.reflect.Tree]]) => q.reflect.Term]) => R,
      ): R =
        caseVal(tpe, body)
    }

    class Method[Q <: Quotes & Singleton, F[_]](using val q: Q)(
      val tpe: qr.MethodType,
      val body: F[(selfSym: qr.Symbol, argss: List[List[qr.Tree]]) => qr.Term],
    ) extends MemberDef[Q, F] {
      override def acceptVisitor[R](
        caseType: (q: Q) ?=> q.reflect.TypeRepr => R,
        caseVal: (q: Q) ?=> (tpe: q.reflect.TypeRepr, body: F[(selfSym: q.reflect.Symbol) => q.reflect.Term]) => R,
        caseMethod: (q: Q) ?=> (tpe: q.reflect.MethodType, body: F[(selfSym: q.reflect.Symbol, argss: List[List[q.reflect.Tree]]) => q.reflect.Term]) => R,
      ): R =
        caseMethod(tpe, body)
    }

    /** Helps with type-inference of the polymorphic function with given arguments. */
    opaque type Poly[Q <: Quotes, M]
      <: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> PreviousSiblings[Q, N] => MemberDef[Q, mode.OutEff]
      =  [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> PreviousSiblings[Q, N] => MemberDef[Q, mode.OutEff]

    def poly[Q <: Quotes, M](using q: Q)(
      f: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> PreviousSiblings[Q, N] => MemberDef[Q, mode.OutEff],
    ): Poly[Q, M] =
      f
  }

  private def newRefinedObject[Base](using baseType: Type[Base])[Q <: Quotes & Singleton](using q: Q)(
    owner : qr.Symbol,
    members : List[(String, MemberDef.Poly[q.type, "term-synth"])],
    anonClassNameSuffix: String,
  ): qr.Term = {
    import qr.*

    val (tpe, termFn) = newRefinedObject_[Base, "term-synth"](members, anonClassNameSuffix)
    val term = termFn(owner)
    Typed(term, TypeTree.of(using tpe.asType))
  }

  private def newRefinedObject_[Base, M](using
    q: Quotes,
    mode: Mode[q.type, M],
    baseType: Type[Base],
  )(
    members : List[(String, MemberDef.Poly[q.type, M])],
    anonClassNameSuffix: String,
  ): (qr.TypeRepr, mode.OutEff[(owner: qr.Symbol) => qr.Term]) = {
    import qr.*

    val baseTypeRepr = TypeRepr.of[Base]
    val selectableBase = AndType(baseTypeRepr, TypeRepr.of[reflect.Selectable])
    val parents =
      if (baseTypeRepr.typeSymbol.flags.is(Flags.Trait))
        List(TypeRepr.of[Object], baseTypeRepr, TypeRepr.of[reflect.Selectable])
      else
        List(baseTypeRepr, TypeRepr.of[reflect.Selectable])

    (
      refinementType(selectableBase) { b =>
        val (_, b1) =
          members.foldLeft((PreviousSiblings.empty["type-synth"](using q), b)) {
            case ((ctx, b), (name, defn)) =>
              defn["type-synth"](ctx) match
                case _: MemberDef.Type[q] =>
                  val (b1, ref) = b.addAbstractType(name)
                  (ctx.addType(name, ref), b1)
                case tm: MemberDef.Val[q, tref] =>
                  val (b1, ref) = b.addMember(name, tm.tpe)
                  (ctx.addTerm(name, ref), b1)
                case md: MemberDef.Method[q, tref] =>
                  val (b1, ref) = b.addMember(name, md.tpe)
                  (ctx.addTerm(name, ref), b1)
          }

        b1.result
      },

      mode.isTermSynth.map { case TypeEq(Refl()) =>
        summon[M =:= "term-synth"]
        { (owner: Symbol) =>
          val clsSym =
            Symbol.newClass(
              owner,
              name = "$anon_" + anonClassNameSuffix,
              parents = parents,
              decls = selfSym => {
                val (_, symsRev) =
                  members.foldLeft((
                    PreviousSiblings.empty["type-synth"](using q),
                    List.empty[Symbol],
                  )) { case ((ctx, acc), (name, defn)) =>
                    defn["type-synth"](ctx) match
                      case td: MemberDef.Type[q] =>
                        val tp = td.body
                        val tpSym =
                          Symbol.newTypeAlias(
                            parent = selfSym,
                            name = name,
                            flags = Flags.EmptyFlags,
                            tpe = tp,
                            privateWithin = Symbol.noSymbol,
                          )
                        (ctx.addType(name, tpSym.typeRef), tpSym :: acc)
                      case tm: MemberDef.Val[q, term] =>
                        val sym =
                          Symbol.newVal(
                            parent = selfSym,
                            name = name,
                            tpe = tm.tpe,
                            flags = Flags.EmptyFlags,
                            privateWithin = Symbol.noSymbol,
                          )
                        (ctx.addTerm(name, sym.termRef), sym :: acc)
                      case md: MemberDef.Method[q, term] =>
                        val sym =
                          Symbol.newMethod(
                            parent = selfSym,
                            name = name,
                            tpe = md.tpe,
                            flags = Flags.EmptyFlags,
                            privateWithin = Symbol.noSymbol,
                          )
                        (ctx.addTerm(name, sym.termRef), sym :: acc)
                  }

                symsRev.reverse
              },
              selfType = None,
            )

          val definedTypeSymbols: List[Symbol] =
            clsSym.declaredTypes

          val definedTypeMap: Map[String, TypeRef] =
            definedTypeSymbols.map(sym => (sym.name, sym.typeRef)).toMap

          val definedValMap: Map[String, Term] =
            clsSym.declaredFields.map(sym => (sym.name, Ref.term(sym.termRef))).toMap

          // XXX: these are all the definitions, not just _previous_ ones
          val ctx = PreviousSiblings["term-synth"](
            definedTypeMap,
            mode.inTermProper.substituteContra(definedValMap),
          )

          val typeDefs =
            definedTypeSymbols.map(TypeDef(_))

          val valDefs =
            members.flatMap { case (name, defn) =>
              defn["term-synth"](ctx)
                .acceptVisitor[Option[Definition]](
                  caseType = _ => None,
                  caseVal = (_, bodyF) => {
                    val body = mode.outEffId.at(bodyF)
                    val sym = clsSym.declaredField(name)
                    Some(ValDef(sym, Some(body(selfSym = sym))))
                  },
                  caseMethod = (_, bodyF) => {
                    val body = mode.outEffId.at(bodyF)
                    val sym =
                      clsSym.declaredMethod(name) match
                        case m :: Nil => m
                        case Nil => report.errorAndAbort(s"Bug: Method `$name` not found in declared methods")
                        case _ => report.errorAndAbort(s"Bug: Multiple methods named `$name` found in declared methods")
                    Some(DefDef(sym, argss => Some(body(sym, argss))))
                  }
                )
            }

          val clsDef = ClassDef(
            clsSym,
            parents = parents.map(t => TypeTree.of(using t.asType)),
            body = typeDefs ++ valDefs,
          )

          val instance =
            Apply(Select(New(TypeIdent(clsSym)), clsSym.primaryConstructor), Nil)

          Block(
            List(clsDef),
            instance,
          )
        }
      },
    )
  }

  private def refinementType(using q: Quotes)(
    baseType: qr.TypeRepr,
  )(
    f: RefinementTypeBuilder[q.type] => qr.TypeRepr
  ): qr.TypeRepr =
    qr.RecursiveType { self =>
      f(RefinementTypeBuilder[q.type](q)(self, baseType))
    }

  private class RefinementTypeBuilder[Q <: Quotes](
    val q: Q,
  )(
    self: q.reflect.RecursiveType,
    acc: q.reflect.TypeRepr
  ) {
    import q.reflect.*

    def addAbstractType(name: String): (RefinementTypeBuilder[Q], TypeRef) = {
      val acc1 = Refinement(acc, name, TypeBounds.empty)
      val ref = typeRef(using q)(self.recThis, name)

      (RefinementTypeBuilder(q)(self, acc1), ref)
    }

    def addMember(name: String, tpe: TypeRepr): (RefinementTypeBuilder[Q], TermRef) = {
      val acc1 = Refinement(acc, name, tpe)
      val ref = TermRef(self.recThis, name)
      (RefinementTypeBuilder(q)(self, acc1), ref)
    }

    def result: TypeRepr =
      acc
  }

  private def typeRef(using q: Quotes)(prefix: qr.TypeRepr, name: String): qr.TypeRef = {
    // XXX: using compiler internals will backfire at some point
    import dotty.tools.dotc.core.{Names, Types}

    Types.TypeRef(
      prefix.asInstanceOf[Types.Type],
      Names.typeName(name),
    )(using
      q.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    ).asInstanceOf[qr.TypeRef]
  }
}
