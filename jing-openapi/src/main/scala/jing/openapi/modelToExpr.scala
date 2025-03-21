package jing.openapi

import jing.openapi.model.*
import scala.quoted.*
import libretto.lambda.Items1Named
import libretto.lambda.util.{Exists, SingletonValue}

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
      val (exp, tpe) = quotedSchematic(s, [A] => sa => quotedSchema(sa))
      given Type[T] = tpe
      ('{ Schema.Proper($exp) }, tpe)
    case u: Schema.Unknown[reason] =>
      quotedSchemaOops(u.reason)

private def quotedSchemaOops[S <: String](s: SingletonValue[S])(using Quotes): (Expr[Schema[Oops[S]]], Type[Oops[S]]) =
  val (exp, tpe) = quotedSingletonString(s)
  given Type[S] = tpe
  ('{ Schema.unknown($exp) }, Type.of[Oops[S]])

private transparent inline def qr(using q: Quotes): q.reflect.type =
  q.reflect

def quotedSchemaFromProto(using Quotes)(
  types: Map[String, qr.TypeRepr],
  prevSchemas: Map[String, qr.TermRef],
  schema: ProtoSchema.Oriented,
): Exists[[T] =>> (Expr[Schema[T]], Type[T])] =
  schema match
    case ProtoSchema.Oriented.Proper(value) =>
      ???
    case ProtoSchema.Oriented.BackwardRef(schemaName) =>
      ???
    case ProtoSchema.Oriented.ForwardRef(schemaName, cycle) =>
      val msg = s"Unsupported recursive schema: ${cycle.mkString(" -> ")}"
      Exists(quotedSchemaOops(SingletonValue(msg)))
    case ProtoSchema.Oriented.UnresolvedRef(schemaName) =>
      val msg = s"Unresolved schema $schemaName"
      Exists(quotedSchemaOops(SingletonValue(msg)))
    case ProtoSchema.Oriented.Unsupported(details) =>
      Exists(quotedSchemaOops(SingletonValue(details)))

def quotedSchematic[F[_], T](
  s: Schematic[F, T],
  f: [A] => F[A] => (Expr[F[A]], Type[A]),
)(using
  Quotes,
  Type[F],
): (Expr[Schematic[F, T]], Type[T]) =
  quotedSchematicRel[F, T, =:=](s, [A] => fa => Exists(summon[A =:= A], f(fa))) match
    case Exists.Some((ev, res)) =>
      ev.substituteContra[[X] =>> (Expr[Schematic[F, X]], Type[X])](res)

def quotedSchematicRel[F[_], T, Rel[_, _]](
  s: Schematic[F, T],
  f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Expr[F[B]], Type[B]))],
)(using
  Quotes,
  Type[F],
  Substitutive[Rel],
): Exists[[U] =>> (Rel[T, U], (Expr[Schematic[F, U]], Type[U]))] =
  val Rel = summon[Substitutive[Rel]]
  s match
    case Schematic.I64() =>
      Exists((Rel.refl[Int64], ('{ Schematic.I64() }, Type.of[Int64])))
    case Schematic.S() =>
      Exists((Rel.refl[Str], ('{ Schematic.S() }, Type.of[Str])))
    case a: Schematic.Array[s, a] =>
      f(a.elem) match
        case e @ Exists.Some((rel, (sb, tb))) =>
          given Type[e.T] = tb
          Exists((rel.lift[Arr], ('{ Schematic.Array($sb) }, Type.of[Arr[e.T]])))
    case o: Schematic.Object[s, ps] =>
      quotedObjectSchematicRel(o, f) match
        case e @ Exists.Some((rel, (s, t))) =>
          given Type[e.T] = t
          Exists(rel.lift[Obj], (s, Type.of[Obj[e.T]]))

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
  val (expr, tp) = quotedObjectSchematic(Schema.asObject(s), [A] => sa => quotedSchema(sa))
  given Type[Ps] = tp
  ('{ Schema.Proper[Obj[Ps]]($expr) }, tp)

def quotedObjectSchematic[F[_], Ps](
  s: Schematic.Object[F, Ps],
  f: [A] => F[A] => (Expr[F[A]], Type[A]),
)(using
  Quotes,
  Type[F],
): (Expr[Schematic.Object[F, Ps]], Type[Ps]) =
  quotedObjectSchematicRel[F, Ps, =:=](s, [A] => fa => Exists((summon[A =:= A], f(fa)))) match
    case Exists.Some((ev, res)) =>
      ev.substituteContra[[Qs] =>> (Expr[Schematic.Object[F, Qs]], Type[Qs])](res)

def quotedObjectSchematicRel[F[_], Ps, Rel[_, _]](
  s: Schematic.Object[F, Ps],
  f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Expr[F[B]], Type[B]))],
)(using
  Quotes,
  Type[F],
  Substitutive[Rel],
): Exists[[Qs] =>> (Rel[Ps, Qs], (Expr[Schematic.Object[F, Qs]], Type[Qs]))] =
  val Rel = summon[Substitutive[Rel]]
  s match
    case Schematic.Object.Empty() =>
      Exists(Rel.refl[{}], ('{ Schematic.Object.Empty() }, Type.of[{}]))
    case snoc @ Schematic.Object.Snoc(init, pname, ptype) =>
      quotedObjectSnocSchematicRel(snoc, f)

private def quotedObjectSnocSchematicRel[F[_], Init, PropName <: String, PropType, Rel[_, _]](
  snoc: Schematic.Object.Snoc[F, Init, PropName, PropType],
  f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Expr[F[B]], Type[B]))],
)(using
  Quotes,
  Type[F],
  Substitutive[Rel],
): Exists[[Qs] =>> (
  Rel[Init || PropName :: PropType, Qs],
  (
    Expr[Schematic.Object[F, Qs]],
    Type[Qs],
  )
)] = {
  (
    quotedObjectSchematicRel(snoc.init, f),
    f(snoc.ptype),
  ) match {
    case (e1 @ Exists.Some((ri, (si, ti))), e2 @ Exists.Some((rl, (sl, tl)))) =>
      type As = e1.T
      type B  = e2.T
      given Type[As] = ti
      given Type[B] = tl

      val (spn, nt) = quotedSingletonString(snoc.pname)

      given Type[PropName] = nt

      val expr: Expr[Schematic.Object[F, As || PropName :: B]] =
        '{ Schematic.Object.Snoc[F, As, PropName, B]($si, $spn, $sl) }

      Exists((
        ri.lift[[X] =>> X || PropName :: PropType] andThen rl.lift[[Y] =>> As || PropName :: Y],
        (expr, Type.of[As || PropName :: B])
      ))
  }
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
