package jing.openapi

import jing.openapi.model.*
import scala.quoted.*
import libretto.lambda.Items1Named
import libretto.lambda.util.{Applicative, Exists, SingletonValue}
import libretto.lambda.util.Applicative.*

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
): (Type[HttpEndpoint[I, O]], Expr[HttpEndpoint[I, O]]) =
  val HttpEndpoint(path, meth, req, resp) = x
  val (reqType, reqExpr) = quotedRequestSchema(req)
  val (respType, respExpr) = quotedResponseSchema(resp)

  given Type[I] = reqType
  given Type[O] = respType

  (
    Type.of[HttpEndpoint[I, O]],
    '{ HttpEndpoint(${Expr(path)}, ${Expr(meth)}, ${reqExpr}, ${respExpr}) },
  )

def quotedRequestSchema[T](
  x: RequestSchema[T],
)(using
  Quotes,
): (Type[T], Expr[RequestSchema[T]]) =
  x match
    case RequestSchema.NoInput =>
      summon[Unit =:= T]
      (Type.of[Unit], '{ RequestSchema.NoInput })
    case ps: RequestSchema.Params[ps] =>
      val (t, s) = quotedObjectSchema(ps.schema)
      given Type[ps] = t
      (Type.of[Obj[ps]], '{ RequestSchema.Params($s) })
    case RequestSchema.Body(schema) =>
      val (t, s) = quotedBodySchema(schema)
      given Type[T] = t
      (t, '{ RequestSchema.Body($s)})
    case pb: RequestSchema.ParamsAndBody[ps, b] =>
      summon[T =:= Obj[{} || "params" :: Obj[ps] || "body" :: b]]
      val (pst, ps) = quotedObjectSchema(pb.params)
      val (bt, b) = quotedBodySchema(pb.body)
      given Type[ps] = pst
      given Type[b] = bt
      (
        Type.of[Obj[{} || "params" :: Obj[ps] || "body" :: b]],
        '{ RequestSchema.ParamsAndBody($ps, $b) },
      )

def quotedResponseSchema[T](
  x: ResponseSchema[T],
)(using
  Quotes,
): (Type[T], Expr[ResponseSchema[T]]) =
  x match
    case ResponseSchema(byStatusCode) =>
      val (t, e) =
        quotedProduct(
          byStatusCode,
          [A] => bs => quotedBodySchema(bs),
        )
      given Type[T] = t
      (t, '{ ResponseSchema($e) })

def quotedBodySchema[T](
  s: BodySchema[T],
)(using
  Quotes,
): (Type[T], Expr[BodySchema[T]]) =
  s match
    case BodySchema.EmptyBody =>
      (Type.of[Unit], '{ BodySchema.EmptyBody })
    case v: BodySchema.Variants[cases] =>
      val (t, e) =
        quotedProduct(
          v.byMediaType,
          [A] => s => quotedSchema(s),
        )
      given Type[cases] = t
      (
        Type.of[DiscriminatedUnion[cases]],
        '{ BodySchema.Variants($e) },
      )


def quotedSchema[T](s: Schema[T])(using Quotes): (Type[T], Expr[Schema[T]]) =
  s match
    case Schema.Proper(s) =>
      val (tpe, exp) = quotedSchematic(s, [A] => sa => quotedSchema(sa))
      given Type[T] = tpe
      (tpe, '{ Schema.Proper($exp) })
    case u: Schema.Unknown[reason] =>
      quotedSchemaOops(u.reason)

private def quotedSchemaOops[S <: String](s: SingletonValue[S])(using Quotes): (Type[Oops[S]], Expr[Schema[Oops[S]]]) =
  val (tpe, exp) = quotedSingletonString(s)
  given Type[S] = tpe
  (Type.of[Oops[S]], '{ Schema.unknown($exp) })

private transparent inline def qr(using q: Quotes): q.reflect.type =
  q.reflect

trait SchemaLookup {
  def lookup(schemaName: String): Exists[[T] =>> (Type[T], Expr[Schema[T]])]
}

def quotedSchemaFromProto(using Quotes)(
  schema: ProtoSchema.Oriented,
): Reader[SchemaLookup, Exists[[T] =>> (Type[T], Expr[Schema[T]])]] = {
  import quotes.reflect.*

  schema match
    case ProtoSchema.Oriented.Proper(value) =>
      quotedSchematicRelA(
        value,
        [A] => ps => quotedSchemaFromProto(ps) map {
          case Exists.Some(te) => Exists((Unrelated(), te))
        },
      ) map {
        case ex @ Exists.Some(_, (tpe, expr)) =>
          given Type[ex.T] = tpe
          Exists((tpe, '{ Schema.Proper($expr) }))
        }

    case ProtoSchema.Oriented.BackwardRef(schemaName) =>
      Reader(_.lookup(schemaName))

    case ProtoSchema.Oriented.ForwardRef(schemaName, cycle) =>
      val msg = s"Unsupported recursive schema: ${cycle.mkString(" -> ")}"
      Reader.pure(
        Exists(quotedSchemaOops(SingletonValue(msg)))
      )

    case ProtoSchema.Oriented.UnresolvedRef(schemaName) =>
      val msg = s"Unresolved schema $schemaName"
      Reader.pure(
        Exists(quotedSchemaOops(SingletonValue(msg)))
      )

    case ProtoSchema.Oriented.Unsupported(details) =>
      Reader.pure(
        Exists(quotedSchemaOops(SingletonValue(details)))
      )
}

def quotedSchematic[F[_], T](
  s: Schematic[F, T],
  f: [A] => F[A] => (Type[A], Expr[F[A]]),
)(using
  Quotes,
  Type[F],
): (Type[T], Expr[Schematic[F, T]]) =
  quotedSchematicRel[F, T, F, =:=](s, [A] => fa => Exists(summon[A =:= A], f(fa))) match
    case Exists.Some((ev, res)) =>
      ev.substituteContra[[X] =>> (Type[X], Expr[Schematic[F, X]])](res)

def quotedSchematicRel[F[_], T, G[_], Rel[_, _]](
  s: Schematic[F, T],
  f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))],
)(using
  Quotes,
  Type[G],
  Substitutive[Rel],
): Exists[[U] =>> (Rel[T, U], (Type[U], Expr[Schematic[G, U]]))] =
  quotedSchematicRelA[F, T, G, Rel, [x] =>> x](s, f)

def quotedSchematicRelA[F[_], T, G[_], Rel[_, _], M[_]](
  s: Schematic[F, T],
  f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))]],
)(using
  Quotes,
  Type[G],
  Substitutive[Rel],
  Applicative[M],
): M[Exists[[U] =>> (Rel[T, U], (Type[U], Expr[Schematic[G, U]]))]] =
  val Rel = summon[Substitutive[Rel]]
  val M = summon[Applicative[M]]
  s match
    case Schematic.I64() =>
      M.pure(
        Exists((Rel.refl[Int64], (Type.of[Int64], '{ Schematic.I64() })))
      )
    case Schematic.S() =>
      M.pure(
        Exists((Rel.refl[Str], (Type.of[Str], '{ Schematic.S() })))
      )
    case a: Schematic.Array[s, a] =>
      f(a.elem) map:
        case e @ Exists.Some((rel, (tb, sb))) =>
          given Type[e.T] = tb
          Exists((rel.lift[Arr], (Type.of[Arr[e.T]], '{ Schematic.Array($sb) })))
    case o: Schematic.Object[s, ps] =>
      quotedObjectSchematicRelA(o, f) map:
        case e @ Exists.Some((rel, (t, s))) =>
          given Type[e.T] = t
          Exists(rel.lift[Obj], (Type.of[Obj[e.T]], s))

def quotedProduct[F[_], Items](
  p: Items1Named.Product[||, ::, F, Items],
  f: [A] => F[A] => (Type[A], Expr[F[A]]),
)(using
  Quotes,
  Type[F],
): (Type[Items], Expr[Items1Named.Product[||, ::, F, Items]]) =
  p match
    case s: Items1Named.Product.Single[sep, of, f, lbl, a] =>
      val (ta, fa) = f(s.value)
      given Type[a] = ta

      val (tl, l) = quotedSingletonString(s.label)
      given Type[lbl] = tl

      (
        Type.of[lbl :: a],
        '{ Items1Named.Product.Single[sep, of, f, lbl, a]($l, $fa)},
      )
    case s: Items1Named.Product.Snoc[sep, of, f, init, lbl, a] =>
      val (tInit, fInit) = quotedProduct(s.init, f)
      given Type[init] = tInit

      val (ta, fa) = f(s.lastElem)
      given Type[a] = ta

      val (tl, l) = quotedSingletonString(s.lastName)
      given Type[lbl] = tl

      (
        Type.of[init || lbl :: a],
        '{ Items1Named.Product.Snoc[sep, of, f, init, lbl, a]($fInit, $l, $fa) },
      )

def quotedObjectSchema[Ps](s: Schema[Obj[Ps]])(using Quotes): (Type[Ps], Expr[Schema[Obj[Ps]]]) =
  val (tp, expr) = quotedObjectSchematic(Schema.asObject(s), [A] => sa => quotedSchema(sa))
  given Type[Ps] = tp
  (tp, '{ Schema.Proper[Obj[Ps]]($expr) })

def quotedObjectSchematic[F[_], Ps](
  s: Schematic.Object[F, Ps],
  f: [A] => F[A] => (Type[A], Expr[F[A]]),
)(using
  Quotes,
  Type[F],
): (Type[Ps], Expr[Schematic.Object[F, Ps]]) =
  quotedObjectSchematicRel[F, Ps, F, =:=](s, [A] => fa => Exists((summon[A =:= A], f(fa)))) match
    case Exists.Some((ev, res)) =>
      ev.substituteContra[[Qs] =>> (Type[Qs], Expr[Schematic.Object[F, Qs]])](res)

def quotedObjectSchematicRel[F[_], Ps, G[_], Rel[_, _]](
  s: Schematic.Object[F, Ps],
  f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))],
)(using
  q: Quotes,
  tg: Type[G],
  Rel: Substitutive[Rel],
): Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], Expr[Schematic.Object[G, Qs]]))] =
  quotedObjectSchematicRelA[F, Ps, G, Rel, [x] =>> x](s, f)

def quotedObjectSchematicRelA[F[_], Ps, G[_], Rel[_, _], M[_]](
  s: Schematic.Object[F, Ps],
  f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))]],
)(using
  q: Quotes,
  tg: Type[G],
  Rel: Substitutive[Rel],
  M: Applicative[M],
): M[Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], Expr[Schematic.Object[G, Qs]]))]] =
  s match
    case Schematic.Object.Empty() =>
      M.pure(
        Exists(Rel.refl[{}], (Type.of[{}], '{ Schematic.Object.Empty() }))
      )
    case snoc @ Schematic.Object.Snoc(init, pname, ptype) =>
      quotedObjectSnocSchematicRelA(snoc, f)

private def quotedObjectSnocSchematicRelA[F[_], Init, PropName <: String, PropType, G[_], Rel[_, _], M[_]](
  snoc: Schematic.Object.Snoc[F, Init, PropName, PropType],
  f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))]],
)(using
  Quotes,
  Type[G],
  Substitutive[Rel],
  Applicative[M],
): M[Exists[[Qs] =>> (
  Rel[Init || PropName :: PropType, Qs],
  (
    Type[Qs],
    Expr[Schematic.Object[G, Qs]],
  )
)]] = {
  summon[Applicative[M]].map2(
    quotedObjectSchematicRelA(snoc.init, f),
    f(snoc.ptype),
  ) {
    case (e1 @ Exists.Some((ri, (ti, si))), e2 @ Exists.Some((rl, (tl, sl)))) =>
      type As = e1.T
      type B  = e2.T
      given Type[As] = ti
      given Type[B] = tl

      val (nt, spn) = quotedSingletonString(snoc.pname)

      given Type[PropName] = nt

      val expr: Expr[Schematic.Object[G, As || PropName :: B]] =
        '{ Schematic.Object.Snoc[G, As, PropName, B]($si, $spn, $sl) }

      Exists((
        ri.lift[[X] =>> X || PropName :: PropType] andThen rl.lift[[Y] =>> As || PropName :: Y],
        (Type.of[As || PropName :: B], expr)
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
): (Type[T], Expr[SingletonValue[T]]) = {
  import quotes.reflect.*

  val (tpe, trm) = quotedStringLiteral(x.value)
  given Type[T] = x.witness.substituteContra(tpe)

  (
    Type.of[T],
    '{ SingletonValue($trm) }.asExprOf[SingletonValue[T]],
  )
}

given stringSingletonToExpr[T <: String]: ToExpr[SingletonValue[T]] with {
  override def apply(x: SingletonValue[T])(using Quotes): Expr[SingletonValue[T]] =
    quotedSingletonString(x)._2
}

private def quotedStringLiteral(s: String)(using Quotes): (Type[s.type], Expr[s.type]) = {
  import quotes.reflect.*

  val term = Literal(StringConstant(s))
  val tpe = term.tpe.asType.asInstanceOf[Type[s.type]]
  (tpe, term.asExprOf(using tpe))
}
