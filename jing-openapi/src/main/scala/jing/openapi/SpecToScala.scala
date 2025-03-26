package jing.openapi

import io.swagger.parser.OpenAPIParser
import jing.openapi.model.{
  ||,
  ::,
  BodySchema,
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
import libretto.lambda.util.{Exists, SingletonValue}
import scala.collection.immutable.{:: as NonEmptyList}
import scala.collection.JavaConverters.*
import scala.quoted.*
import scala.annotation.tailrec

private[openapi] object SpecToScala {
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
        "schemas" -> { _ =>
          class SchemaLookupImpl(ctx: PreviousSiblings[q.type]) extends SchemaLookup[Function0] {
            override def lookup(schemaName: String): Exists[[T] =>> (Type[T], () => Expr[Schema[T]])] =
              val tpe = ctx.types(schemaName).asType.asInstanceOf[Type[Any]]
              def go[T](using Type[T]): Expr[Schema[T]] =
                Select.unique(Ref.term(ctx.terms(schemaName)), "schema")
                  .asExprOf[Schema[T]]
              Exists((tpe, () => go(using tpe)))
          }

          def resolveSchema(
            ctx: PreviousSiblings[q.type],
            s: ProtoSchema.Oriented,
          ): Exists[[T] =>> (Type[T], Function0[Expr[Schema[T]]])] =
            quotedSchemaFromProto[Function0](s)
              .run(SchemaLookupImpl(ctx))

          val (tpe, bodyFn) = newRefinedObject_[AnyRef](
            members = schemas.flatMap { case (name, s) =>
              List(
                // "opaque" type alias
                name -> { ctx =>
                  val tpe = resolveSchema(ctx, s).value._1
                  MemberDef.Type(TypeRepr.of(using tpe))
                },

                // companion "object"
                name -> { ctx =>
                  val ex = resolveSchema(ctx, s)
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
        "paths" -> { prevSiblings =>
          val schemasField = prevSiblings.terms.getOrElse("schemas", { throw AssertionError("field `schemas` not previously defined") })
          val schemaTerms: Map[String, TermRef] =
            schemas
              .map { case (name, _) => (name, TermRef(TermRef(schemasField, name), "schema")) }
              .toMap

          val (tpe, bodyFn) = newRefinedObject_[AnyRef](
            members = paths.map { case (path, pathItem) =>
              path -> { _ =>
                val (tpe, bodyFn) = pathToObject(schemaTerms, path, pathItem)
                MemberDef.Val(tpe, bodyFn)
              }
            },
            "paths",
          )
          MemberDef.Val(tpe, bodyFn)
        },
      ),
      "Api",
    ).asExpr
  }

  /**
    * @tparam A the abstract ("opaque") alias (of `T`) for which we are creating the companion object
    * @tparam T the definition of `A`
    */
  private def schemaCompanion[A, T](using Quotes, Type[A], Type[T])(
    name: String,
    schema: () => Expr[Schema[T]],
  ): (qr.TypeRepr, (owner: qr.Symbol) => qr.Term) = {
    import qr.*

    newRefinedObject_[SchemaCompanion[A, T]](
      members = List(
        // schema of the "opaque" type
        "schema" -> { _ =>
          MemberDef.Val(TypeRepr.of[Schema[A]], _ => schema().asTerm)
        },

        // constructor of the "opaque" type
        "apply" -> { _ =>
          MemberDef.Method(
            MethodType(paramNames = List("x"))(
              self => List(TypeRepr.of[Value[T]]),
              self => TypeRepr.of[Value[A]],
            ),
            body = { (self, argss) =>
              // implementation is just identity
              val List(List(x)) = argss
              x.asExpr.asTerm
            },
          )
        },

        // deconstructor, overrides unapply from TotalExtractor
        "unapply" -> { _ =>
          MemberDef.Method(
            MethodType(paramNames = List("x"))(
              self => List(TypeRepr.of[Value[A]]),
              self => TypeRepr.of[Some[Value[T]]],
            ),
            body = { (self, argss) =>
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

  private transparent inline def qr(using q: Quotes): q.reflect.type =
    q.reflect

  private def pathToObject(using Quotes)(
    schemas: Map[String, qr.TermRef],
    path: String,
    pathItem: io.swagger.v3.oas.models.PathItem,
  ): (qr.TypeRepr, (owner: qr.Symbol) => qr.Term) = {
    val operations =
      HttpMethod.values.toList
        .flatMap { m => pathOperation(pathItem, m).map((m, _)) }

    newRefinedObject_[AnyRef](
      members = operations.map { case (meth, op) =>
        (meth.toString, _ => {
          val (tp, bodyFn) = operationToObject(schemas, path, meth, op)
          MemberDef.Val(tp, bodyFn)
        })
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

  private type ObjSchema[A] = Schema[Obj[A]]

  private def operationToObject(using Quotes)(
    schemas: Map[String, qr.TermRef],
    path: String,
    method: HttpMethod,
    op: io.swagger.v3.oas.models.Operation,
  ): (qr.TypeRepr, (owner: qr.Symbol) => qr.Term) = {
    import qr.*

    val paramSchema: Option[Exists[ObjSchema]] =
      Option(op.getParameters())
        .map(_.asScala.toList)
        .collect { case p :: ps => NonEmptyList(p, ps) }
        .map(parametersSchema(schemas, _))

    val reqBodySchema =
      Option(op.getRequestBody())
        .map(requestBodySchema(schemas, _))

    val reqSchema: RequestSchema[?] =
      requestSchema(paramSchema, reqBodySchema)

    val responseSchema =
      Option(op.getResponses())
        .map(_.entrySet().iterator().asScala.map(e => (e.getKey(), e.getValue())).toList)
        .collect { case r :: rs => NonEmptyList(r, rs) }
        .map(responseBodyByStatus(schemas, _))
        .getOrElse {
          report.errorAndAbort(s"No response defined for $method $path")
        }

    val endpoint = HttpEndpoint(path, method, reqSchema, responseSchema)

    val (tpe, expr) = quotedHttpEndpoint(endpoint)
    (TypeRepr.of(using tpe), _ => expr.asTerm)
  }

  private def parametersSchema(using Quotes)(
    schemas: Map[String, qr.TermRef],
    params: NonEmptyList[io.swagger.v3.oas.models.parameters.Parameter],
  ): Exists[ObjSchema] = {
    val res =
      params.foldLeft[Schematic.Object[Schema, ?]](Schematic.Object.Empty()) { (acc, p) =>
        val pSchema = protoSchemaToSchema(schemas, protoSchema(p.getSchema()))
        Schematic.Object.snoc(acc, p.getName(), pSchema)
      }
    Exists.Some(Schema.Proper(res))
  }

  private def requestBodySchema(using Quotes)(
    schemas: Map[String, qr.TermRef],
    requestBody: io.swagger.v3.oas.models.parameters.RequestBody,
  ): BodySchema[?] =
    bodySchema(schemas, requestBody.getContent())

  private def bodySchema(using Quotes)(
    schemas: Map[String, qr.TermRef],
    nullableContent: io.swagger.v3.oas.models.media.Content,
  ): BodySchema[?] =
    bodyVariants(schemas, nullableContent)
      .map(BodySchema.Variants(_))
      .getOrElse(BodySchema.EmptyBody)

  private def bodyVariants(using Quotes)(
    schemas: Map[String, qr.TermRef],
    nullableContent: io.swagger.v3.oas.models.media.Content,
  ): Option[Items1Named.Product[||, ::, Schema, ?]] =
    Option(nullableContent)
      .map(_.entrySet().iterator().asScala.map(e => (e.getKey(), e.getValue())).toList)
      .collect { case r :: rs => NonEmptyList(r, rs) }
      .map { _.mapToProduct(mt => Exists(protoSchemaToSchema(schemas, protoSchema(mt.getSchema())))) }

  private def responseBodyByStatus(using Quotes)(
    schemas: Map[String, qr.TermRef],
    byStatus: NonEmptyList[(String, io.swagger.v3.oas.models.responses.ApiResponse)],
  ): ResponseSchema[?] =
    ResponseSchema(
      byStatus
        .mapToProduct[BodySchema] { apiResponse =>
          Exists(responseBodySchema(schemas, apiResponse))
        },
    )

  private def responseBodySchema(using Quotes)(
    schemas: Map[String, qr.TermRef],
    apiResponse: io.swagger.v3.oas.models.responses.ApiResponse,
  ): BodySchema[?] =
    bodySchema(schemas, apiResponse.getContent())

  private def requestSchema(
    paramsSchema: Option[Exists[ObjSchema]],
    reqBodySchema: Option[BodySchema[?]],
  ): RequestSchema[?] =
    (paramsSchema, reqBodySchema) match
      case (Some(ps), Some(bs)) => RequestSchema.ParamsAndBody(ps.value, bs)
      case (Some(ps), None    ) => RequestSchema.Params(ps.value)
      case (None    , Some(bs)) => RequestSchema.Body(bs)
      case (None    , None    ) => RequestSchema.NoInput

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

  private def protoSchemaToSchema(using Quotes)(
    schemas: Map[String, qr.TermRef],
    schema: ProtoSchema,
  ): Schema[?] = {
    schema match
      case ProtoSchema.Proper(value) =>
        Schema.Proper(
          value.wipeTranslate[Schema]([A] => sa => Exists(protoSchemaToSchema(schemas, sa)))
        )
      case ProtoSchema.Ref(name) =>
        Schema.unknown(reason = s"Local schema $name not yet supported")
      case ProtoSchema.Unsupported(details) =>
        Schema.unknown(reason = details)
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
              case Exists.Some(fa) =>
                go(
                  Items1Named.Product.Snoc(acc, SingletonValue(tag), fa),
                  as,
                )

      val NonEmptyList((tag, a), tail) = as

      go(
        Items1Named.Product.Single(SingletonValue(tag), f(a).value),
        tail,
      )
    }

  }

  private class PreviousSiblings[Q <: Quotes & Singleton](using val q: Q)(
    val types: Map[String, qr.TypeRef],
    val terms: Map[String, qr.TermRef],
  ) {
    def addType(name: String, value: qr.TypeRef): PreviousSiblings[Q] =
      PreviousSiblings(
        types.updated(name, value),
        terms,
      )

    def addTerm(name: String, value: qr.TermRef): PreviousSiblings[Q] =
      PreviousSiblings(
        types,
        terms.updated(name, value),
      )
  }

  private object PreviousSiblings {
    def empty(using q: Quotes): PreviousSiblings[q.type] =
      PreviousSiblings(Map.empty, Map.empty)
  }

  private sealed trait MemberDef[Q <: Quotes]
  private object MemberDef {
    class Type[Q <: Quotes & Singleton](using val q: Q)(
      val body: qr.TypeRepr,
    ) extends MemberDef[Q]

    class Val[Q <: Quotes & Singleton](using val q: Q)(
      val tpe: qr.TypeRepr,
      val body: (selfSym: qr.Symbol) => qr.Term,
    ) extends MemberDef[Q]

    class Method[Q <: Quotes & Singleton](using val q: Q)(
      val tpe: qr.MethodType,
      val body: (selfSym: qr.Symbol, argss: List[List[qr.Tree]]) => qr.Term,
    ) extends MemberDef[Q]
  }

  private def newRefinedObject[Base](using q: Quotes, baseType: Type[Base])(
    owner : qr.Symbol,
    members : List[(String, PreviousSiblings[q.type] => MemberDef[q.type])],
    anonClassNameSuffix: String,
  ): qr.Term = {
    import qr.*

    val (tpe, termFn) = newRefinedObject_[Base](members, anonClassNameSuffix)
    val term = termFn(owner)
    Typed(term, TypeTree.of(using tpe.asType))
  }

  private def newRefinedObject_[Base](using q: Quotes, baseType: Type[Base])(
    members: List[(String, PreviousSiblings[q.type] => MemberDef[q.type])],
    anonClassNameSuffix: String,
  ): (qr.TypeRepr, (owner: qr.Symbol) => qr.Term) = {
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
          members.foldLeft((PreviousSiblings.empty(using q), b)) {
            case ((ctx, b), (name, defn)) =>
              defn(ctx) match
                case _: MemberDef.Type[q] =>
                  val (b1, ref) = b.addAbstractType(name)
                  (ctx.addType(name, ref), b1)
                case tm: MemberDef.Val[q] =>
                  val (b1, ref) = b.addMember(name, tm.tpe)
                  (ctx.addTerm(name, ref), b1)
                case md: MemberDef.Method[q] =>
                  val (b1, ref) = b.addMember(name, md.tpe)
                  (ctx.addTerm(name, ref), b1)
          }

        b1.result
      },

      { (owner: Symbol) =>
        val clsSym =
          Symbol.newClass(
            owner,
            name = "$anon_" + anonClassNameSuffix,
            parents = parents,
            decls = selfSym => {
              val (_, symsRev) =
                members.foldLeft((
                  PreviousSiblings.empty(using q),
                  List.empty[Symbol],
                )) { case ((ctx, acc), (name, defn)) =>
                  defn(ctx) match
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
                    case tm: MemberDef.Val[q] =>
                      val sym =
                        Symbol.newVal(
                          parent = selfSym,
                          name = name,
                          tpe = tm.tpe,
                          flags = Flags.EmptyFlags,
                          privateWithin = Symbol.noSymbol,
                        )
                      (ctx.addTerm(name, sym.termRef), sym :: acc)
                    case md: MemberDef.Method[q] =>
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

        val definedValMap: Map[String, TermRef] =
          clsSym.declaredFields.map(sym => (sym.name, sym.termRef)).toMap

        // XXX: these are all the definitions, not just _previous_ ones
        val ctx = PreviousSiblings(definedTypeMap, definedValMap)

        val typeDefs =
          definedTypeSymbols.map(TypeDef(_))

        val valDefs =
          members.flatMap { case (name, defn) =>
            defn(ctx) match
              case _: MemberDef.Type[q] =>
                None
              case vd: MemberDef.Val[q] =>
                val sym = clsSym.declaredField(name)
                Some(ValDef(sym, Some(vd.body(selfSym = sym))))
              case md: MemberDef.Method[q] =>
                val sym =
                  clsSym.declaredMethod(name) match
                    case m :: Nil => m
                    case Nil => report.errorAndAbort(s"Bug: Method `$name` not found in declared methods")
                    case _ => report.errorAndAbort(s"Bug: Multiple methods named `$name` found in declared methods")
                Some(DefDef(sym, argss => Some(md.body(sym, argss))))
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
    import dotty.tools.dotc.core.{Names, Types} // XXX: using compiler internals will backfire at some point

    def addAbstractType(name: String): (RefinementTypeBuilder[Q], TypeRef) = {
      val acc1 = Refinement(acc, name, TypeBounds.empty)
      val ref =
        Types.TypeRef(
          self.recThis.asInstanceOf[Types.Type],
          Names.typeName(name),
        )(using
          q.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
        ).asInstanceOf[TypeRef]

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
}
