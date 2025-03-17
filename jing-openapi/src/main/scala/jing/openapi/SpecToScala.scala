package jing.openapi

import io.swagger.parser.OpenAPIParser
import jing.openapi.model.{||, ::, BodySchema, HttpEndpoint, HttpMethod, Obj, OpenApiSpec, RequestSchema, ResponseSchema, Schema}
import libretto.lambda.Items1Named
import libretto.lambda.util.{Exists, SingletonValue}
import scala.collection.immutable.{:: as NonEmptyList}
import scala.collection.JavaConverters.*
import scala.quoted.*
import scala.annotation.tailrec

private[openapi] object SpecToScala {
  def apply(location: String)(using Quotes): Expr[Any] = {
    import quotes.reflect.*

    val spec = new OpenAPIParser().readLocation(location, null, null).getOpenAPI()

    val schemas: List[(String, io.swagger.v3.oas.models.media.Schema[?])] = {
      val b = List.newBuilder[(String, io.swagger.v3.oas.models.media.Schema[?])]
      spec.getComponents().getSchemas().forEach { (name, schema) => b += ((name, schema)) }
      b.result()
    }

    val paths: List[(String, io.swagger.v3.oas.models.PathItem)] = {
      val b = List.newBuilder[(String, io.swagger.v3.oas.models.PathItem)]
      spec.getPaths().forEach { (name, path) => b += ((name, path)) }
      b.result()
    }

    newRefinedObject[OpenApiSpec](
      owner = Symbol.spliceOwner,
      opaqTypes = Nil,
      vals = List(
        ("schemas", { (ctx: Map[String, TypeRef], _) =>
          newRefinedObject_[AnyRef](
            opaqTypes = schemas.map { case (name, s) => (name, ctx1 => schemaToType(ctx ++ ctx1, s)) },
            vals = Nil, // TODO: companion vals with smart constructors
          )
        }),
        ("paths", { (ctx: Map[String, TypeRef], _) =>
          newRefinedObject_[AnyRef](
            opaqTypes = Nil,
            vals = paths.map { case (path, pathItem) =>
              (path, (ctx1, _) => pathToObject(ctx ++ ctx1, path, pathItem))
            },
          )
        }),
      ),
    ).asExpr
  }

  private transparent inline def qr(using q: Quotes): q.reflect.type =
    q.reflect

  private def schemaToType(using Quotes)(
    ctx: Map[String, qr.TypeRef],
    schema: io.swagger.v3.oas.models.media.Schema[?],
  ): qr.TypeRepr = {
    // TODO
    qr.TypeRepr.of[Unit]
  }

  private def pathToObject(using Quotes)(
    ctx: Map[String, qr.TypeRef],
    path: String,
    pathItem: io.swagger.v3.oas.models.PathItem,
  ): (qr.TypeRepr, (owner: qr.Symbol) => qr.Term) = {
    val operations =
      HttpMethod.values.toList
        .flatMap { m => pathOperation(pathItem, m).map((m, _)) }

    newRefinedObject_[AnyRef](
      opaqTypes = Nil,
      vals = operations.map { case (meth, op) =>
        (meth.toString, (ctx1, _) => operationToObject(ctx ++ ctx1, path, meth, op))
      },
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
    ctx: Map[String, qr.TypeRef],
    path: String,
    method: HttpMethod,
    op: io.swagger.v3.oas.models.Operation,
  ): (qr.TypeRepr, (owner: qr.Symbol) => qr.Term) = {
    import qr.*

    val paramSchema: Option[Exists[ObjSchema]] =
      Option(op.getParameters())
        .map(_.asScala.toList)
        .collect { case p :: ps => NonEmptyList(p, ps) }
        .map(parametersSchema)

    val reqBodySchema =
      Option(op.getRequestBody())
        .map(requestBodySchema)

    val reqSchema: RequestSchema[?] =
      requestSchema(paramSchema, reqBodySchema)

    val responseSchema =
      Option(op.getResponses())
        .map(_.entrySet().iterator().asScala.map(e => (e.getKey(), e.getValue())).toList)
        .collect { case r :: rs => NonEmptyList(r, rs) }
        .map(responseBodyByStatus)
        .getOrElse {
          report.errorAndAbort(s"No response defined for $method $path")
        }

    val endpoint = HttpEndpoint(path, method, reqSchema, responseSchema)

    val (expr, tpe) = quotedHttpEndpoint(endpoint)
    (TypeRepr.of(using tpe), _ => expr.asTerm)
  }

  private def parametersSchema(
    params: NonEmptyList[io.swagger.v3.oas.models.parameters.Parameter],
  ): Exists[ObjSchema] = {
    Exists.Some(
      params.foldLeft[Schema.Object[?]](Schema.Object.Empty) { (acc, p) =>
        Schema.Object.snoc(acc, p.getName(), schemaToSchema(p.getSchema()))
      }
    )
  }

  private def requestBodySchema(
    requestBody: io.swagger.v3.oas.models.parameters.RequestBody,
  ): BodySchema[?] =
    bodySchema(requestBody.getContent())

  private def bodySchema(
    nullableContent: io.swagger.v3.oas.models.media.Content,
  ): BodySchema[?] =
    bodyVariants(nullableContent)
      .map(BodySchema.Variants(_))
      .getOrElse(BodySchema.EmptyBody)

  private def bodyVariants(
    nullableContent: io.swagger.v3.oas.models.media.Content,
  ): Option[Items1Named.Product[||, ::, Schema, ?]] =
    Option(nullableContent)
      .map(_.entrySet().iterator().asScala.map(e => (e.getKey(), e.getValue())).toList)
      .collect { case r :: rs => NonEmptyList(r, rs) }
      .map { _.mapToProduct(mt => Exists(schemaToSchema(mt.getSchema()))) }

  private def responseBodyByStatus(
    byStatus: NonEmptyList[(String, io.swagger.v3.oas.models.responses.ApiResponse)],
  ): ResponseSchema[?] =
    ResponseSchema(
      byStatus
        .mapToProduct[BodySchema] { apiResponse =>
          Exists(responseBodySchema(apiResponse))
        },
    )

  private def responseBodySchema(
    apiResponse: io.swagger.v3.oas.models.responses.ApiResponse,
  ): BodySchema[?] =
    bodySchema(apiResponse.getContent())

  private def requestSchema(
    paramsSchema: Option[Exists[ObjSchema]],
    reqBodySchema: Option[BodySchema[?]],
  ): RequestSchema[?] =
    (paramsSchema, reqBodySchema) match
      case (Some(ps), Some(bs)) => RequestSchema.ParamsAndBody(ps.value, bs)
      case (Some(ps), None    ) => RequestSchema.Params(ps.value)
      case (None    , Some(bs)) => RequestSchema.Body(bs)
      case (None    , None    ) => RequestSchema.NoInput

  private def schemaToSchema(
    schema: io.swagger.v3.oas.models.media.Schema[?],
  ): Schema[?] = {
    schema.getType() match {
      case null =>
        schema.get$ref() match
          case null =>
            Schema.unknown(reason = "Schema with no type or $ref")
          case ref =>
            Schema.unknown(reason = s"Schema $$ref not yet supported: $ref")
      case "string" =>
        // TODO: look for modifiers such as format and enum
        Schema.S
      case "array" =>
        val itemSchema = schemaToSchema(schema.getItems())
        Schema.Array(itemSchema)
      case other =>
        Schema.unknown(reason = s"Type '$other' no yet supported.")
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

  private def newRefinedObject[Base](using Quotes, Type[Base])(
    owner      : qr.Symbol,
    opaqTypes  : List[(String, (prevTypeSiblings: Map[String, qr.TypeRef]) => qr.TypeRepr)],
    vals       : List[(String, (prevTypeSiblings: Map[String, qr.TypeRef], prevTermSiblings: Map[String, qr.TermRef]) => (qr.TypeRepr, (selfSym: qr.Symbol) => qr.Term))],
    // TODO: add support for methods
  ): qr.Term = {
    import qr.*

    val (tpe, termFn) = newRefinedObject_[Base](opaqTypes, vals)
    val term = termFn(owner)
    Typed(term, TypeTree.of(using tpe.asType))
  }

  // TODO: shorthand version for when there are no types to introduce
  private def newRefinedObject_[Base](using Quotes, Type[Base])(
    opaqTypes  : List[(String, (prevTypeSiblings: Map[String, qr.TypeRef]) => qr.TypeRepr)],
    vals       : List[(String, (prevTypeSiblings: Map[String, qr.TypeRef], prevTermSiblings: Map[String, qr.TermRef]) => (qr.TypeRepr, (selfSym: qr.Symbol) => qr.Term))],
    // TODO: add support for methods
  ): (qr.TypeRepr, (owner: qr.Symbol) => qr.Term) = {
    import qr.*

    val superClass = TypeRepr.of[Base]

    (
      refinementType(superClass) { b =>
        val (definedTypes, b1) =
          opaqTypes.foldLeft((Map.empty[String, TypeRef], b)) {
            case ((alreadyDefinedTypes, b), (name, _)) =>
              val (b1, ref) = b.addAbstractType(name)
              (alreadyDefinedTypes.updated(name, ref), b1)
          }

        val (_, b2) =
          vals
            .foldLeft((Map.empty[String, TermRef], b1)) {
              case ((alreadyDefinedTerms, b), (name, fn)) =>
                val (tpe, _) = fn(definedTypes, alreadyDefinedTerms)
                val (b1, ref) = b.addMember(name, tpe)
                (alreadyDefinedTerms.updated(name, ref), b1)
            }

        b2.result
      },

      { (owner: Symbol) =>
        val clsSym =
          Symbol.newClass(
            owner,
            name = "$anon",
            parents = List(superClass),
            decls = selfSym => {
              val (declaredTypes, typeSymsRev) =
                opaqTypes.foldLeft((
                  Map.empty[String, TypeRef],
                  List.empty[Symbol],
                )) { case ((alreadyDefinedTypes, acc), (name, fn)) =>
                  val tp = fn(alreadyDefinedTypes)
                  val tpSym =
                    Symbol.newTypeAlias(
                      parent = selfSym,
                      name = name,
                      flags = Flags.EmptyFlags,
                      tpe = tp,
                      privateWithin = Symbol.noSymbol,
                    )
                  (alreadyDefinedTypes.updated(name, tpSym.typeRef), tpSym :: acc)
                }

              val (_, valSymsRev) =
                vals
                  .foldLeft((
                    Map.empty[String, TermRef],
                    List.empty[Symbol],
                  )) { case ((alreadyDefinedVals, acc), (name, fn)) =>
                    val (tpe, body) = fn(declaredTypes, alreadyDefinedVals)
                    val sym =
                      Symbol.newVal(
                        parent = selfSym,
                        name = name,
                        tpe = tpe,
                        flags = Flags.EmptyFlags,
                        privateWithin = Symbol.noSymbol,
                      )
                    (alreadyDefinedVals.updated(name, sym.termRef), sym :: acc)
                  }

              typeSymsRev reverse_::: valSymsRev.reverse
            },
            selfType = None,
          )

        val definedTypeSymbols: List[Symbol] =
          clsSym.declaredTypes

        val definedTypesMap: Map[String, TypeRef] =
          definedTypeSymbols.map(sym => (sym.name, sym.typeRef)).toMap

        val typeDefs =
          definedTypeSymbols.map(TypeDef(_))

        val valDefs =
          vals
            .foldLeft((
              Map.empty[String, TermRef],
              List.empty[ValDef],
            )) { case ((prevTermSiblings, acc), (name, fn)) =>
              val (tpe, body) = fn(definedTypesMap, prevTermSiblings)
              val sym = clsSym.declaredField(name)
              (
                prevTermSiblings.updated(name, sym.termRef),
                ValDef(sym, Some(body(selfSym = sym))) :: acc
              )
            }._2.reverse

        val clsDef = ClassDef(
          clsSym,
          parents = List(TypeTree.of(using superClass.asType)),
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
    baseClass: qr.TypeRepr,
  )(
    f: RefinementTypeBuilder[q.type] => qr.TypeRepr
  ): qr.TypeRepr =
    qr.RecursiveType { self =>
      f(RefinementTypeBuilder[q.type](q)(self, baseClass))
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
      val ref =
        Types.TermRef(
          self.recThis.asInstanceOf[Types.Type],
          Names.termName(name),
        )(using
          q.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
        ).asInstanceOf[TermRef]

      (RefinementTypeBuilder(q)(self, acc1), ref)
    }

    def result: TypeRepr =
      acc
  }
}
