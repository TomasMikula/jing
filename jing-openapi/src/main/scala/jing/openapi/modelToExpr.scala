package jing.openapi

import jing.openapi.model.*
import scala.quoted.*

given ToExpr[HttpMethod] with {
  override def apply(x: HttpMethod)(using Quotes): Expr[HttpMethod] =
    x match
      case HttpMethod.Get     => '{ HttpMethod.Get }
      case HttpMethod.Post    => '{ HttpMethod.Post }
      case HttpMethod.Put     => '{ HttpMethod.Put }
      case HttpMethod.Delete  => '{ HttpMethod.Delete }
      case HttpMethod.Head    => '{ HttpMethod.Head }
      case HttpMethod.Options => '{ HttpMethod.Options }
      case HttpMethod.Patch   => '{ HttpMethod.Patch }
      case HttpMethod.Trace   => '{ HttpMethod.Trace }

}

def quotedHttpEndpoint[I, O](
  x: HttpEndpoint[I, O],
)(using
  Quotes,
): (Expr[HttpEndpoint[I, O]], Type[HttpEndpoint[I, O]]) =
  val HttpEndpoint(path, meth, req, resp) = x
  val (reqExpr, reqType) = quotedRequestSchema(req)
  val (respExpr, respType) = quotedSchema(resp)

  given Type[I] = reqType
  given Type[O] = respType

  ('{ HttpEndpoint(${Expr(path)}, ${Expr(meth)}, ${reqExpr}, ${respExpr}) }, Type.of[HttpEndpoint[I, O]])

def quotedRequestSchema[T](
  x: RequestSchema[T],
)(using
  Quotes,
): (Expr[RequestSchema[T]], Type[T]) =
  x match
    case RequestSchema.NoInput =>
      summon[Unit =:= T]
      ('{ RequestSchema.NoInput }, Type.of[Unit])
    case ps: RequestSchema.Params[ps] =>
      val (s, t) = quotedObjectSchema(ps.schema)
      given Type[ps] = t
      ('{ RequestSchema.Params($s) }, Type.of[Obj[ps]])
    case RequestSchema.Body(schema) =>
      val (s, t) = quotedSchema(schema)
      given Type[T] = t
      ('{ RequestSchema.Body($s)}, t)
    case pb: RequestSchema.ParamsAndBody[ps, b] =>
      summon[T =:= Obj[{} || "params" :: Obj[ps] || "body" :: b]]
      val (ps, pst) = quotedObjectSchema(pb.params)
      val (b, bt) = quotedSchema(pb.body)
      given Type[ps] = pst
      given Type[b] = bt
      (
        '{ RequestSchema.ParamsAndBody($ps, $b) },
        Type.of[Obj[{} || "params" :: Obj[ps] || "body" :: b]]
      )

def quotedSchema[T](s: Schema[T])(using Quotes): (Expr[Schema[T]], Type[T]) =
  s match
    case Schema.I64 =>
        ???
    case Schema.S =>
      ('{ Schema.S }, Type.of[Str])
    case o: Schema.Object[ps] =>
      val (s, t) = quotedObjectSchema(o)
      given Type[ps] = t
      (s, Type.of[Obj[ps]])


def quotedObjectSchema[Ps](s: Schema[Obj[Ps]])(using Quotes): (Expr[Schema[Obj[Ps]]], Type[Ps]) =
  s match
    case Schema.Object.Empty =>
      ('{ Schema.Object.Empty }, Type.of[{}])
    case snoc @ Schema.Object.Snoc(init, pname, ptype) =>
      quotedObjectSnocSchema(snoc)

private def quotedObjectSnocSchema[Init, PropName <: String, PropType](
  snoc: Schema.Object.Snoc[Init, PropName, PropType]
)(using
  Quotes
): (
  Expr[Schema[Obj[Init || PropName :: PropType]]],
  Type[Init || PropName :: PropType],
) =
  val (si, ti) = quotedObjectSchema(snoc.init)
  val (sl, tl) = quotedSchema(snoc.ptype)

  given Type[Init] = ti
  given Type[PropType] = tl
  given nt0: Type[snoc.pname.type] = stringLiteralType(snoc.pname)

  given nt: Type[PropName] =
    snoc.singletonPropName.substituteContra(
      nt0
    )

  val expr: Expr[Schema[Obj[Init || PropName :: PropType]]] =
    '{
      Schema.Object.snoc[Init, PropType](
        $si,
        ${Expr(snoc.pname)},
        $sl,
      )
    }
      .asExprOf[Schema[Obj[Init || PropName :: PropType]]]

  (
    expr,
    Type.of[Init || PropName :: PropType],
  )

private def stringLiteralType(s: String)(using Quotes): Type[s.type] =
  import quotes.reflect.*

  Literal(StringConstant(s)).tpe.asType
    .asInstanceOf[Type[s.type]]

