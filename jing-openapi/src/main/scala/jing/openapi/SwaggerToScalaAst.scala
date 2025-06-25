package jing.openapi

import io.swagger.parser.OpenAPIParser
import java.net.URI
import java.nio.file.Path
import jing.macroUtil.{Mode, StructuralRefinement, qr}
import jing.macroUtil.Mode.IsSubsumedBy
import jing.macroUtil.Mode.IsSubsumedBy.given
import jing.macroUtil.StructuralRefinement.{MemberDef, MemberDefsPoly, PreviousSiblings, typeRefUnsafe}
import jing.openapi.ModelToScalaAst.{*, given}
import jing.openapi.model.{
  ||,
  ::,
  :?,
  BodySchema,
  DiscriminatedUnion,
  EndpointList,
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
import scala.annotation.tailrec
import scala.collection.immutable.{:: as NonEmptyList}
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.util.{Failure, Success, Try}

private[openapi] object SwaggerToScalaAst {
  def apply(location: String)(using q: Quotes): Expr[Any] = {
    import quotes.reflect.*

    val spec =
      resolveLocation(location) match
        case Left(uri) =>
          new OpenAPIParser().readLocation(uri.toString, null, null).getOpenAPI()
        case Right(path) =>
          scala.util.Try { java.nio.file.Files.readString(path) } match
            case Success(str) =>
              new OpenAPIParser().readContents(str, null, null).getOpenAPI()
            case Failure(e) =>
              report.errorAndAbort(s"Failed to read spec from file '$path': $e")

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

    StructuralRefinement.typedTermStateful[OpenApiSpec][q.type][[f[_]] =>> Unit](
      owner = Symbol.spliceOwner,
      members = MemberDefsPoly.emptyUnit[q.type, "term-synth"]
        .next("schemas", MemberDef.PolyS.fromStateless(schemasField["term-synth"](schemas)))
        .next("paths", pathsField["term-synth"](schemas, paths))
        .next("endpointList", endpointsField["term-synth"])
        ,
      "Api",
    )._1.asExpr
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
              MemberDef.Type(TypeRepr.of(using tpe), isAbstract = true)
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
  ): MemberDef.PolyS[q.type, M, [f[_]] =>> Unit, [f[_]] =>> List[(String, List[(HttpMethod, qr.TypeRepr)])]] = {
    import quotes.reflect.*

    MemberDef.PolyS.writer[q.type, M, [f[_]] =>> List[(String, List[(HttpMethod, TypeRepr)])]] { [M1] => (m1, _) ?=> (_, ctx) =>
      val schemasField: ctx.mode.InTerm =
        ctx.terms.getOrElse("schemas", { throw AssertionError("field `schemas` not previously defined") })

      val schemaLookup: SchemaLookup[m1.OutEff] =
        schemaLookupFromSchemaField[M1](Mode.sameInTerm(ctx.mode, m1)(schemasField), schemas.map(_._1))

      type State[F[_]] = List[(String, List[(HttpMethod, TypeRepr)])]
      val init: MemberDefsPoly[q.type, M1, State] =
        MemberDefsPoly.Empty[q.type, M1, State]([N] => (mode: Mode[q.type, N], sub: N IsSubsumedBy M1) ?=> Nil)

      val (tpe, endpoints, bodyFn) = StructuralRefinement.forModeStateful[M1][AnyRef][State](
        members = paths.foldLeft(init) { case (acc, (path, pathItem)) =>
          acc.next(
            path,
            MemberDef.PolyS[q.type, M1, State, State] { [M2] => (m2, sub) ?=> (s, name, _) =>
              val (tpe, endpoints, bodyFn) = pathToObject[M2](schemaLookup.mapK(sub.downgrader), path, pathItem)
              ((name, endpoints) :: s, MemberDef.Val(tpe, bodyFn))
            }
          )
        },
        "paths",
      )
      (endpoints.reverse, MemberDef.Val(tpe, bodyFn))
    }
  }

  private def endpointsField[M](using q: Quotes): MemberDef.PolyS[q.type, M, [f[_]] =>> List[(String, List[(HttpMethod, qr.TypeRepr)])], [f[_]] =>> Unit] = {
    import quotes.reflect.*

    MemberDef.PolyS.reader { [M1] => (m1, sub) ?=> (endpoints, _, ctx) =>
      import m1.applicativeOutEff

      val pathsField: ctx.mode.InTerm =
        ctx.terms.getOrElse("paths", { throw AssertionError("field `paths` not previously defined") })
      val endpointMap: List[(String, List[(HttpMethod, Exists[[T] =>> (Type[T], m1.OutEff[Expr[T]])])])] =
        endpointMapFromPathsField(Mode.sameInTerm(ctx.mode, m1)(pathsField), endpoints)
      val (endpointTypes, namesTuple, typesTuple, expr) =
        endpointMap.foldRight[(TypeRepr, TypeRepr, TypeRepr, m1.OutEff[Expr[EndpointList[?, ? <: NamedTuple.AnyNamedTuple]]])](
          (TypeRepr.of[Void], TypeRepr.of[EmptyTuple], TypeRepr.of[EmptyTuple], '{ EndpointList.empty }.pure[m1.OutEff]),
        ) { case ((path, ops), acc) =>
          ops.foldRight(acc) { case ((method, ep), (esAcc, namesAcc, typesAcc, termAcc)) =>
            val name = s"${path}_${method.nameUpperCase}"
            val (nameTpe, nameExp) = ModelToScalaAst.quotedSingletonString(SingletonType(name))
            given Type[ep.T] = ep.value._1
            val headNameTpe = TypeRepr.of(using nameTpe)
            val headTypeTpe = TypeRepr.of(using ep.value._1)
            val headTpe = TypeRepr.of[::].appliedTo(List(headNameTpe, headTypeTpe))
            (
              TypeRepr.of[||].appliedTo(List(headTpe, esAcc)),
              TypeRepr.of[*:].appliedTo(List(headNameTpe, namesAcc)),
              TypeRepr.of[*:].appliedTo(List(headTypeTpe, typesAcc)),
              (ep.value._2 zipWith termAcc):
                case ('{ $e: HttpEndpoint[a, b] }, termAcc) =>
                  val ntTypeAcc = TypeRepr.of[NamedTuple.NamedTuple].appliedTo(List(namesAcc, typesAcc))
                  def go[Es, T <: NamedTuple.AnyNamedTuple](termAcc: Expr[EndpointList[Es, T]]) =
                    given Type[Es] = esAcc.asType.asInstanceOf[Type[Es]]
                    given Type[T] = ntTypeAcc.asType.asInstanceOf[Type[T]]
                    def go[S <: String](nameExp: Expr[SingletonType[S]])(using Type[S]) =
                      '{ EndpointList.Cons($nameExp, $e, $termAcc) }
                    go(nameExp)(using nameTpe)
                  go((termAcc: Expr[EndpointList[?, ? <: NamedTuple.AnyNamedTuple]]).asInstanceOf[Expr[EndpointList[Any, NamedTuple.AnyNamedTuple]]])
            )
          }
        }
      MemberDef.Val(
        TypeRepr.of[EndpointList].appliedTo(List(
          endpointTypes,
          TypeRepr.of[NamedTuple.NamedTuple].appliedTo(List(namesTuple, typesTuple))
        )),
        expr.map(expr => (owner: Symbol) => expr.asTerm),
      )
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
            val ev = m.outEffConstUnit.flip
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

  private def endpointMapFromPathsField[M](using q: Quotes, m: Mode[q.type, M])(
    pathsField: m.InTerm,
    endpoints: List[(String, List[(HttpMethod, qr.TypeRepr)])],
  ): List[(String, List[(HttpMethod, Exists[[T] =>> (Type[T], m.OutEff[Expr[T]])])])] = {
    import q.reflect.*

    m match {
      case _: Mode.TermSynth[q] =>
        val pathsFieldTerm: Term =
          m.inTermProper(pathsField)
        endpoints
          .map { case (path, ops) =>
            val pathField =
              Select.unique(pathsFieldTerm, "selectDynamic")
                .appliedTo(Literal(StringConstant(path)))
            val pathFieldTyped =
              Select.unique(pathField, "$asInstanceOf$").appliedToType(TypeRepr.of[scala.reflect.Selectable])
            path -> ops.map { case (method, tp) =>
              val endpointTerm =
                Select.unique(pathFieldTyped, "selectDynamic")
                  .appliedTo(Literal(StringConstant(method.toString)))
              val endpointTermTyped =
                Select.unique(endpointTerm, "$asInstanceOf$").appliedToType(tp)
              method -> endpointTypeAndExpr(tp, endpointTermTyped.pure[m.OutEff])
            }
          }

      case _: Mode.TypeSynth[q] =>
        val pathsFieldRef: TermRef =
          m.inTermRefOnly(pathsField)
        val fabricate: [A] => Unit => m.OutEff[A] =
          val ev = m.outEffConstUnit.flip
          [A] => (u: Unit) => ev.at[A](())
        endpoints
          .map { case (path, ops) =>
            path -> ops.map { case (method, tp) =>
              method -> endpointTypeAndExpr(tp, fabricate(()))
            }
          }
    }
  }

  private def typeAndSchemaExpr[F[_]](using Quotes)(
    tpe: qr.TypeRepr,
    schemaTerm: F[qr.Term],
  )(using Functor[F]): Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])] = {
    tpe.asType match
      case '[t] => Indeed((Type.of[t], schemaTerm.map(_.asExprOf[Schema[t]])))
  }

  private def endpointTypeAndExpr[F[_]](using Quotes)(
    endpointType: qr.TypeRepr,
    endpointTerm: F[qr.Term],
  )(using Functor[F]): Exists[[T] =>> (Type[T], F[Expr[T]])] =
    endpointType.asType match
      case '[t] => Indeed((Type.of[t], endpointTerm.map(_.asExprOf[t])))

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
  ): (qr.TypeRepr, List[(HttpMethod, qr.TypeRepr)], mode.OutEff[(owner: qr.Symbol) => qr.Term]) = {
    import quotes.reflect.*

    val operations =
      HttpMethod.values.toList
        .flatMap { m => pathOperation(pathItem, m).map((m, _)) }

    type State[F[_]] = List[(HttpMethod, TypeRepr)]
    val init: MemberDefsPoly[q.type, M, State] =
      MemberDefsPoly.Empty[q.type, M, State]([N] => (mode: Mode[q.type, N], sub: N IsSubsumedBy M) ?=> Nil)

    val (tp, endpoints, bodyFn) =
      StructuralRefinement.forModeStateful[M][AnyRef][State](
        members = operations.foldLeft(init) { case (acc, (meth, op)) =>
          acc.next(
            meth.toString,
            MemberDef.PolyS[q.type, M, State, State] { [N] => (_, sub) ?=> (s, _, _) =>
              operationToObject(schemas.mapK(sub.downgrader), path, meth, op) match
                case Indeed((tp, body)) =>
                  val tr = TypeRepr.of(using tp)
                  (
                    (meth, tr) :: s,
                    MemberDef.Val(tr, body.map { b => (_: Symbol) => b.asTerm })
                  )
            }
          )
        },
        path,
      )

    (tp, endpoints.reverse, bodyFn)
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

  private type QuotedPathParamSchema[F[_]] =
    Exists[[T] =>> (Type[T], F[Expr[RequestSchema.Path.ParamSchema[T]]])]

  private type QuotedQueryParamSchema[F[_]] =
    Exists[[T] =>> (Type[T], F[Expr[RequestSchema.Params.QueryParamSchema[T]]])]

  private enum ProtoParam[F[_]]:
    case PathParam(name: String, schema: QuotedPathParamSchema[F])
    case QueryParam(name: String, schema: QuotedQueryParamSchema[F], required: Boolean)
    case Unsupported(name: String, required: Boolean, reason: ProtoParam.UnsupportedReason)
    case Error(name: String, required: Boolean, details: String)

  private object ProtoParam {
    enum UnsupportedReason:
      case UnsupportedLocation(location: String)
      case ContentFieldNotSupported
  }

  private def resolveParam[F[_]](
    param: io.swagger.v3.oas.models.parameters.Parameter,
  )(using
    q: Quotes,
    F: Applicative[F],
  ): ProtoParam[F[_]] = {
    import ProtoParam.{PathParam, QueryParam}
    import ProtoParam.UnsupportedReason.*

    val name = param.getName

    val required: Boolean =
      param.getRequired match
        case null => false
        case other => other

    param.getSchema match
      case null =>
        param.getContent match
          case null =>
            ProtoParam.Error(name, required, details = "parameter must have either schema or content")
          case _ =>
            ProtoParam.Unsupported(name, required, reason = ContentFieldNotSupported)

      case nnSchema =>
        val schema: ProtoSchema =
          protoSchema(nnSchema)

        val style = Option(param.getStyle).map(_.toString)
        val explode = Option(param.getExplode).map(b => b: Boolean)

        param.getIn match
          case "path" =>
            PathParam(name, toQuotedPathParamSchema(schema, style, explode))
          case "query" =>
            QueryParam(name, toQuotedQueryParamSchema(schema, style, explode), required)
          case other =>
            ProtoParam.Unsupported(name, required, UnsupportedLocation(other))
  }

  private def toQuotedPathParamSchema[F[_]](
    schema: ProtoSchema,
    style: Option[String],
    explode: Option[Boolean],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): QuotedPathParamSchema[F] =
    quotedPathParamSchema(toPathParamSchema(schema, style, explode)) match
      case (t, e) => Indeed((t, F.pure(e)))

  private def toQuotedQueryParamSchema[F[_]](
    schema: ProtoSchema,
    style: Option[String],
    explode: Option[Boolean],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): QuotedQueryParamSchema[F] =
    quotedQueryParamSchema(toQueryParamSchema(schema, style, explode)) match
      case (t, e) => Indeed((t, F.pure(e)))

  private def toPathParamSchema(
    schema: ProtoSchema,
    style: Option[String],
    explode: Option[Boolean],
  ): RequestSchema.Path.ParamSchema[?] =
    schema match
      case ProtoSchema.Proper(s) =>
        RequestSchema.Path.ParamSchema.Format(style, explode) match
          case Left(unsupported) =>
            unsupported
          case Right(format) =>
            s match
              case p: SchemaMotif.Primitive[Schema, t] =>
                RequestSchema.Path.ParamSchema.Primitive(p.recast, format)
              case SchemaMotif.Array(elem) =>
                RequestSchema.Path.ParamSchema.unsupported("array-typed parameters not supported in path")
              case _: SchemaMotif.Object[f, ps] =>
                RequestSchema.Path.ParamSchema.unsupported("object-typed parameters not supported in path")
      case ProtoSchema.Unsupported(message) =>
        RequestSchema.Path.ParamSchema.unsupported(message)
      case ProtoSchema.Ref(schemaName) =>
        RequestSchema.Path.ParamSchema.unsupported(s"schema refs not supported in path parameters, found $schemaName")

  private def toQueryParamSchema(
    schema: ProtoSchema,
    style: Option[String],
    explode: Option[Boolean],
  ): RequestSchema.Params.QueryParamSchema[?] =
    schema match
      case ProtoSchema.Proper(s) =>
        RequestSchema.Params.QueryParamSchema.Format(style, explode) match
          case Left(unsupported) =>
            unsupported
          case Right(format) =>
            s match
              case p: SchemaMotif.Primitive[f, t] =>
                RequestSchema.Params.QueryParamSchema.Primitive(p.recast, format)
              case SchemaMotif.Array(elem) =>
                elem match
                  case ProtoSchema.Proper(elem) =>
                    elem match
                      case p: SchemaMotif.Primitive[f, t] =>
                        RequestSchema.Params.QueryParamSchema.PrimitiveArray(p.recast, format)
                      case SchemaMotif.Array(elem) =>
                        RequestSchema.Params.QueryParamSchema.unsupported("nested arrays not supported in query parameters")
                      case _: SchemaMotif.Object[f, ps] =>
                        RequestSchema.Params.QueryParamSchema.unsupported("arrays of objects not supported in query parameters")
                  case ProtoSchema.Ref(schemaName) =>
                    RequestSchema.Params.QueryParamSchema.unsupported(s"schema refs not supported in query parameters, found $schemaName")
                  case ProtoSchema.Unsupported(message) =>
                    RequestSchema.Params.QueryParamSchema.unsupported(message)
              case _: SchemaMotif.Object[f, ps] =>
                RequestSchema.Params.QueryParamSchema.unsupported("object-typed parameters not supported in query")
      case ProtoSchema.Ref(schemaName) =>
        RequestSchema.Params.QueryParamSchema.unsupported(s"schema refs not supported in query parameters, found $schemaName")
      case ProtoSchema.Unsupported(message) =>
        RequestSchema.Params.QueryParamSchema.unsupported(message)

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
          .map(resolveParam(_))
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

  // Even though the `F` parameter is not really needed at the moment,
  // it will be needed when we start resolving parameter references.
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
        case p: (ProtoParam.QueryParam[F] | ProtoParam.Unsupported[F] | ProtoParam.Error[F]) => Right(p)

    val (queryParams, params2) =
      params1.partitionMap:
        case p: ProtoParam.QueryParam[F] => Left(p)
        case p: (ProtoParam.Unsupported[F] | ProtoParam.Error[F]) => Right(p)

    // TODO: support header params

    val unsupportedAsQueryParams: List[ProtoParam.QueryParam[F]] =
      params2.map { p =>
        val (name, required, msg) =
          p match
            case ProtoParam.Unsupported(name, required, reason) =>
              import ProtoParam.UnsupportedReason.*
              val msg = reason match
                case UnsupportedLocation(location) => s"'$location' parameters are not yet supported"
                case ContentFieldNotSupported => s"'content' field not yet supported. Use schema field instead."
              (name, required, msg)
            case ProtoParam.Error(name, required, msg) =>
              (name, required, msg)
        val schema = RequestSchema.Params.QueryParamSchema.unsupported(msg)
        val (tp, exp) = quotedQueryParamSchema(schema)
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
    val ProtoParam.PathParam(name, schema) = param

    def go[PName <: String](
      pname: SingletonType[PName],
    ): Exists[[Ps] =>> (Type[Ps], F[Expr[RequestSchema.Path.Parameterized[Ps]]])] =
      val (nmt, nme) = ModelToScalaAst.quotedSingletonString(pname)
      given Type[PName] = nmt

      schema match
        case ex @ Indeed((t, fSch)) =>
          given Type[ex.T] = t
          Indeed((
            Type.of[Init || PName :: ex.T],
            F.map2(init, fSch) { (init, sch) => '{ RequestSchema.Path.WithParam($init, $nme, $sch, ${Expr(suffix)}) } },
          ))

    go[name.type](SingletonType(name))
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
    quotedNamedProductUnrelatedAA(
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
    quotedNamedProductUnrelatedAA(
      byStatus.asProduct,
      [A] => apiResponse => Reader((sl: SchemaLookup[F]) => responseBodySchemaOrPlainText(sl, apiResponse)),
    ).run(schemas) match {
      case x @ Indeed((tp, bs)) =>
        given Type[x.T] = tp
        Indeed((
          Type.of[x.T],
          bs.map { bs => '{ResponseSchema($bs) } }
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
      case None => Indeed((Type.of[Unit], F.pure('{ BodySchema.Empty })))

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
        schema.getEnum match
          case null =>
            // TODO: look for format modifier
            ProtoSchema.str
          case vals =>
            vals.asScala.toList.map(_.asInstanceOf[String]) match
              case Nil                 => ProtoSchema.Unsupported("empty enum")
              case NonEmptyList(v, vs) => ProtoSchema.strEnum(v, vs*)
      case "integer" =>
        schema.getFormat() match
          case "int32" =>
            schema.getEnum match
              case null =>
                ProtoSchema.i32
              case vals =>
                // TODO: detect and report the parser giving use Longs if values don't fit into Int range
                vals.asScala.toList.map(_.asInstanceOf[Integer].toInt) match
                  case Nil                 => ProtoSchema.Unsupported("empty enum")
                  case NonEmptyList(v, vs) => ProtoSchema.int32Enum(v, vs*)
          case "int64" =>
            schema.getEnum match
              case null =>
                ProtoSchema.i64
              case vals =>
                // using Number instead of Long, as the parser uses Integer if all cases fit into Int
                vals.asScala.toList.map(_.asInstanceOf[java.lang.Number].longValue()) match
                  case Nil                 => ProtoSchema.Unsupported("empty enum")
                  case NonEmptyList(v, vs) => ProtoSchema.int64Enum(v, vs*)
          case other =>
            ProtoSchema.Unsupported(s"Unsupported integer format: $other")
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

  private def resolveLocation(location: String)(using Quotes): Either[URI, Path] =
    import scala.util.Try
    import quotes.reflect.*

    Try { new URI(location) } match
      case Success(uri) =>
        if (uri.getScheme != null)
          // proper URI
          Left(uri)
        else
          Try { Path.of(location) } match
            case Success(p) =>
              Position.ofMacroExpansion.sourceFile.getJPath match
                case Some(sourceFile) =>
                  Right(sourceFile.getParent.resolve(p))
                case None =>
                  // no source file, we are in a REPL.
                  // Use the current working directory to resolve paths
                  System.getProperty("user.dir") match
                    case null =>
                      report.errorAndAbort(s"Cannot resolve relative path '$location': neither source file nor working directory are known")
                    case base =>
                      Right(Path.of(base).resolve(p))
            case Failure(e) =>
              report.errorAndAbort(s"The given location has no URI schema and is not a valid path: '$location' (${e.getMessage})")
      case Failure(e) =>
        report.errorAndAbort(s"Not a proper URI or path: '$location'")
}
