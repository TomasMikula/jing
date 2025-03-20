package jing.openapi

import jing.openapi.model.*
import scala.quoted.*
import libretto.lambda.Items1Named
import libretto.lambda.util.SingletonValue

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
  val (respExpr, respType) = quotedResponseSchema(resp)

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
      val (s, t) = quotedBodySchema(schema)
      given Type[T] = t
      ('{ RequestSchema.Body($s)}, t)
    case pb: RequestSchema.ParamsAndBody[ps, b] =>
      summon[T =:= Obj[{} || "params" :: Obj[ps] || "body" :: b]]
      val (ps, pst) = quotedObjectSchema(pb.params)
      val (b, bt) = quotedBodySchema(pb.body)
      given Type[ps] = pst
      given Type[b] = bt
      (
        '{ RequestSchema.ParamsAndBody($ps, $b) },
        Type.of[Obj[{} || "params" :: Obj[ps] || "body" :: b]]
      )

def quotedResponseSchema[T](
  x: ResponseSchema[T],
)(using
  Quotes,
): (Expr[ResponseSchema[T]], Type[T]) =
  x match
    case ResponseSchema(byStatusCode) =>
      val (e, t) =
        quotedProduct(
          byStatusCode,
          [A] => bs => quotedBodySchema(bs),
        )
      given Type[T] = t
      ('{ ResponseSchema($e) }, t)

def quotedBodySchema[T](
  s: BodySchema[T],
)(using
  Quotes,
): (Expr[BodySchema[T]], Type[T]) =
  s match
    case BodySchema.EmptyBody =>
      ('{ BodySchema.EmptyBody }, Type.of[Unit])
    case v: BodySchema.Variants[cases] =>
      val (e, t) =
        quotedProduct(
          v.byMediaType,
          [A] => s => quotedSchema(s),
        )
      given Type[cases] = t
      (
        '{ BodySchema.Variants($e) },
        Type.of[DiscriminatedUnion[cases]],
      )


def quotedSchema[T](s: Schema[T])(using Quotes): (Expr[Schema[T]], Type[T]) =
  s match
    case Schema.Proper(s) =>
      val (exp, tpe) = quotedSchematic(s)
      given Type[T] = tpe
      ('{ Schema.Proper($exp) }, tpe)
    case u: Schema.Unknown[reason] =>
      val (exp, tpe) = quotedSingletonString(u.reason)
      given Type[reason] = tpe
      ('{ Schema.unknown($exp) }, Type.of[Oops[reason]])

def quotedSchematic[T](s: Schematic[Schema, T])(using Quotes): (Expr[Schematic[Schema, T]], Type[T]) =
  s match
    case Schematic.I64() =>
      ('{ Schematic.I64() }, Type.of[Int64])
    case Schematic.S() =>
      ('{ Schematic.S() }, Type.of[Str])
    case a: Schematic.Array[s, a] =>
      val (sa, ta) = quotedSchema(a.elem)
      given Type[a] = ta
      ('{ Schematic.Array($sa) }, Type.of[Arr[a]])
    case o: Schematic.Object[s, ps] =>
      val (s, t) = quotedObjectSchematic(o)
      given Type[ps] = t
      (s, Type.of[Obj[ps]])

def quotedProduct[F[_], Items](
  p: Items1Named.Product[||, ::, F, Items],
  f: [A] => F[A] => (Expr[F[A]], Type[A]),
)(using
  Quotes,
  Type[F],
): (Expr[Items1Named.Product[||, ::, F, Items]], Type[Items]) =
  p match
    case s: Items1Named.Product.Single[sep, of, f, lbl, a] =>
      val (fa, ta) = f(s.value)
      given Type[a] = ta

      val (l, tl) = quotedSingletonString(s.label)
      given Type[lbl] = tl

      (
        '{ Items1Named.Product.Single[sep, of, f, lbl, a]($l, $fa)},
        Type.of[lbl :: a]
      )
    case s: Items1Named.Product.Snoc[sep, of, f, init, lbl, a] =>
      val (fInit, tInit) = quotedProduct(s.init, f)
      given Type[init] = tInit

      val (fa, ta) = f(s.lastElem)
      given Type[a] = ta

      val (l, tl) = quotedSingletonString(s.lastName)
      given Type[lbl] = tl

      (
        '{ Items1Named.Product.Snoc[sep, of, f, init, lbl, a]($fInit, $l, $fa) },
        Type.of[init || lbl :: a]
      )

def quotedObjectSchema[Ps](s: Schema[Obj[Ps]])(using Quotes): (Expr[Schema[Obj[Ps]]], Type[Ps]) =
  val (expr, tp) = quotedObjectSchematic(Schema.asObject(s))
  given Type[Ps] = tp
  ('{ Schema.Proper[Obj[Ps]]($expr) }, tp)

def quotedObjectSchematic[Ps](s: Schematic.Object[Schema, Ps])(using Quotes): (Expr[Schematic.Object[Schema, Ps]], Type[Ps]) =
  s match
    case Schematic.Object.Empty() =>
      ('{ Schematic.Object.Empty() }, Type.of[{}])
    case snoc @ Schematic.Object.Snoc(init, pname, ptype) =>
      quotedObjectSnocSchematic(snoc)

private def quotedObjectSnocSchematic[Init, PropName <: String, PropType](
  snoc: Schematic.Object.Snoc[Schema, Init, PropName, PropType]
)(using
  Quotes
): (
  Expr[Schematic.Object[Schema, Init || PropName :: PropType]],
  Type[Init || PropName :: PropType],
) = {
  val (si, ti) = quotedObjectSchematic(snoc.init)
  val (sl, tl) = quotedSchema(snoc.ptype)

  given Type[Init] = ti
  given Type[PropType] = tl

  val (spn, nt) = quotedSingletonString(snoc.pname)

  given Type[PropName] = nt

  val expr: Expr[Schematic.Object[Schema, Init || PropName :: PropType]] =
    '{ Schematic.Object.Snoc[Schema, Init, PropName, PropType]($si, $spn, $sl) }

  (expr, Type.of[Init || PropName :: PropType])
}

private def prodSingle[F[_], Label <: String, T](
  label: SingletonValue[Label],
  value: F[T],
): Items1Named.Product[||, ::, F, Label :: T] =
  Items1Named.Product.Single(label, value)

private def quotedSingletonString[T <: String](x: SingletonValue[T])(using
  Quotes,
): (Expr[SingletonValue[T]], Type[T]) = {
  import quotes.reflect.*

  val (trm, tpe) = quotedStringLiteral(x.value)
  given Type[T] = x.witness.substituteContra(tpe)

  (
    '{ SingletonValue($trm) }.asExprOf[SingletonValue[T]],
    Type.of[T],
  )
}

given stringSingletonToExpr[T <: String]: ToExpr[SingletonValue[T]] with {
  override def apply(x: SingletonValue[T])(using Quotes): Expr[SingletonValue[T]] =
    quotedSingletonString(x)._1
}

private def quotedStringLiteral(s: String)(using Quotes): (Expr[s.type], Type[s.type]) = {
  import quotes.reflect.*

  val term = Literal(StringConstant(s))
  val tpe = term.tpe.asType.asInstanceOf[Type[s.type]]
  (term.asExprOf(using tpe), tpe)
}
