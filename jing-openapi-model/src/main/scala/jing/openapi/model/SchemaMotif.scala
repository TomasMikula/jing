package jing.openapi.model

import libretto.lambda.Items1
import libretto.lambda.util.{Applicative, Exists, SingletonType}
import libretto.lambda.util.Exists.Indeed

/** Schema structure parametric in the type of nested schemas.
 *
 * @tparam F representation of nested schemas. In [[Schema]],
 *   [[Schematic]] is used with `F` recursively instantiated to [[Schema]] itself.
 */
sealed trait SchemaMotif[F[_], A] {
  import SchemaMotif.*

  def translate[G[_]](h: [X] => F[X] => G[X]): SchemaMotif[G, A] =
    this match
      case p: Primitive[G, A] => p.recast[G]
      case Array(elem) => Array(h(elem))
      case Object(value) => Object:
        value.translate[G, G](
          [A] => fa => h(fa),
          [A] => fa => h(fa),
        )

  def wipeTranslate[G[_]](h: [X] => F[X] => Exists[G]): SchemaMotif[G, ?] =
    this match
      case p: Primitive[F, A] => p.recast[G]
      case Array(elem) => Array(h(elem).value)
      case Object(value) => Object:
        value.wipeTranslate[G, G](
          [A] => fa => h(fa),
          [A] => fa => h(fa),
        ).value

  def wipeTranslateA[M[_], G[_]](h: [X] => F[X] => M[Exists[G]])(using M: Applicative[M]): M[SchemaMotif[G, ?]] =
    this match
      case p: Primitive[F, A] => M.pure(p.recast[G])
      case Array(elem) => h(elem).map(el => Array(el.value))
      case Object(value) =>
        value.wipeTranslateA[M, G, G](
          [A] => fa => h(fa),
          [A] => fa => h(fa),
        )
          .map(o => Object(o.value))

  def isNotOops[S](using A =:= Oops[S]): Nothing =
    throw AssertionError("Impossible: Schemas for type Oops[S] are not representable by SchemaMotif")
}

object SchemaMotif {
  sealed trait Primitive[F[_], T] extends SchemaMotif[F, T] {
    def recast[G[_]]: Primitive[G, T] = this.asInstanceOf
  }

  sealed trait BasicPrimitive[F[_], T] extends Primitive[F, T] {
    def makeValue(v: ScalaReprOf[T]): Value[T] =
      this match
        case I32() => Value.int32(v)
        case I64() => Value.int64(v)
        case S() => Value.str(v)
        case B() => Value.bool(v)
  }

  case class I32[F[_]]() extends BasicPrimitive[F, Int32]
  case class I64[F[_]]() extends BasicPrimitive[F, Int64]
  case class S[F[_]]() extends BasicPrimitive[F, Str]
  case class B[F[_]]() extends BasicPrimitive[F, Bool]

  case class Enumeration[F[_], T, Cases](
    baseType: BasicPrimitive[F, T],
    cases: Items1.Product[||, Void, ScalaValueOf[_, T], Cases],
  ) extends Primitive[F, Enum[T, Cases]] {

    def find(a: ScalaReprOf[T]): Option[ScalaValueOf[? <: a.type & ScalaUnionOf[Cases], T]] = {

      def go[Cases](
        cases: Items1.Product[||, Void, ScalaValueOf[_, T], Cases],
      ): Option[ScalaValueOf[? <: a.type & ScalaUnionOf[Cases], T]] =
        import libretto.lambda.Items1.Product.{Single, Snoc}

        cases match
          case s: Single[sep, nil, f, c] =>
            val ev: c =:= ScalaUnionOf[Cases] =
              summon[(Void || c) =:= Cases].liftCo[ScalaUnionOf]
            ev.substituteCo[[x] =>> Option[ScalaValueOf[a.type & x, T]]]:
              s.value.contains(a)
          case s: Snoc[sep, nil, f, init, last] =>
            val ev: last <:< ScalaUnionOf[Cases] =
              summon[(init || last) =:= Cases].liftCo[ScalaUnionOf]

            ev.substituteCo[[x] =>> Option[ScalaValueOf[? <: a.type & x, T]]]:
              s.last.contains(a)
            .orElse:
              val ev2: ScalaUnionOf[init] <:< ScalaUnionOf[Cases] =
                summon[(init || last) =:= Cases].liftCo[ScalaUnionOf]
              ev2.substituteCo[[x] =>> Option[ScalaValueOf[? <: a.type & x, T]]]:
                go[init](s.init)

      go(cases)
    }

    def scalaValues: List[ScalaReprOf[T]] =
      cases.toList([a] => (va: ScalaValueOf[a, T]) => va.get)
  }

  case class Array[F[_], T](elem: F[T]) extends SchemaMotif[F, Arr[T]]

  case class Object[F[_], Ps](
    value: ObjectMotif[F, F, Ps],
  ) extends SchemaMotif[F, Obj[Ps]]

  object Object {
    def empty[F[_]]: Object[F, Void] =
      Object(ObjectMotif.Empty())

    def snoc[F[_], Init, K <: String, V](
      init: SchemaMotif[F, Obj[Init]],
      pname: SingletonType[K],
      ptype: F[V],
    ): Object[F, Init || K :: V] =
      Object(ObjectMotif.Snoc(asObject(init).value, pname, ptype))

    def snoc[F[_], Init, PropType](
      init: SchemaMotif[F, Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Object[F, Init || pname.type :: PropType] =
      snoc(init, SingletonType(pname), ptype)

    def snocOpt[F[_], Init, K <: String, V](
      init: SchemaMotif[F, Obj[Init]],
      pname: SingletonType[K],
      ptype: F[V],
    ): Object[F, Init || K :? V] =
      Object(ObjectMotif.SnocOpt(asObject(init).value, pname, ptype))

    def snocOpt[F[_], Init, PropType](
      init: SchemaMotif[F, Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Object[F, Init || pname.type :? PropType] =
      snocOpt(init, SingletonType(pname), ptype)
  }

  extension [F[_], Ps](s: SchemaMotif[F, Obj[Ps]])
    def asObject: SchemaMotif.Object[F, Ps] =
      s match
        case o: Object[F, Ps] => o
}
