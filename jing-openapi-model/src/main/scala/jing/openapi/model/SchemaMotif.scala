package jing.openapi.model

import libretto.lambda.Items1
import libretto.lambda.util.{Applicative, Exists, SingletonType}

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
      case Object.Empty() => Object.Empty()
      case Object.Snoc(init, pname, ptype) => Object.Snoc(asObject(init.translate(h)), pname, h(ptype))
      case Object.SnocOpt(init, pname, ptype) => Object.SnocOpt(asObject(init.translate(h)), pname, h(ptype))

  def wipeTranslate[G[_]](h: [X] => F[X] => Exists[G]): SchemaMotif[G, ?] =
    this match
      case p: Primitive[F, A] => p.recast[G]
      case Array(elem) => Array(h(elem).value)
      case o: Object[f, ps] => o.wipeTranslateObj(h).value

  def wipeTranslateA[G[_], H[_]](h: [X] => F[X] => G[Exists[H]])(using G: Applicative[G]): G[SchemaMotif[H, ?]] =
    this match
      case p: Primitive[F, A] => G.pure(p.recast[H])
      case Array(elem) => h(elem).map(el => Array(el.value))
      case o: Object[f, ps] => o.wipeTranslateObjA(h).map(_.value)

  def isNotOops[S](using A =:= Oops[S]): Nothing =
    throw AssertionError("Impossible: Schemas for type Oops[S] are not representable by SchemaMotif")
}

object SchemaMotif {
  sealed trait Primitive[F[_], T] extends SchemaMotif[F, T] {
    def recast[G[_]]: Primitive[G, T] = this.asInstanceOf
  }
  case class I32[F[_]]() extends Primitive[F, Int32]
  case class I64[F[_]]() extends Primitive[F, Int64]
  case class S[F[_]]() extends Primitive[F, Str]
  case class B[F[_]]() extends Primitive[F, Bool]

  case class Enumeration[F[_], T, Cases](
    baseType: Primitive[F, T],
    cases: Items1.Product[||, Void, ScalaValueOf[_, T], Cases],
  ) extends Primitive[F, Enum[T, Cases]]

  case class Array[F[_], T](elem: F[T]) extends SchemaMotif[F, Arr[T]]

  sealed trait Object[F[_], Ps] extends SchemaMotif[F, Obj[Ps]] {
    private[SchemaMotif] def wipeTranslateObj[G[_]](h: [X] => F[X] => Exists[G]): Exists[[X] =>> SchemaMotif[G, Obj[X]]] =
      this match
        case Object.Empty() =>
          Exists(Object.Empty())
        case Object.Snoc(init, pname, ptype) =>
          Exists(Object.Snoc(asObject(init.wipeTranslateObj(h).value), pname, h(ptype).value))
        case Object.SnocOpt(init, pname, ptype) =>
          Exists(Object.SnocOpt(asObject(init.wipeTranslateObj(h).value), pname, h(ptype).value))

    private[SchemaMotif] def wipeTranslateObjA[G[_], H[_]](
      h: [X] => F[X] => G[Exists[H]],
    )(using
      G: Applicative[G],
    ): G[Exists[[X] =>> SchemaMotif[H, Obj[X]]]] =
      this match
        case Object.Empty() =>
          G.pure(Exists(Object.Empty()))
        case Object.Snoc(init, pname, ptype) =>
          G.map2(
            init.wipeTranslateObjA(h),
            h(ptype),
          ) { (init, ptype) =>
            Exists(Object.Snoc(asObject(init.value), pname, ptype.value))
          }
        case Object.SnocOpt(init, pname, ptype) =>
          G.map2(
            init.wipeTranslateObjA(h),
            h(ptype),
          ) { (init, ptype) =>
            Exists(Object.SnocOpt(asObject(init.value), pname, ptype.value))
          }
  }
  object Object {
    case class Empty[F[_]]() extends Object[F, Void]

    sealed trait NonEmpty[F[_], Ps] extends Object[F, Ps]

    case class Snoc[F[_], Init, PropName <: String, PropType](
      init: Object[F, Init],
      pname: SingletonType[PropName],
      ptype: F[PropType],
    ) extends Object.NonEmpty[F, Init || PropName :: PropType]

    case class SnocOpt[F[_], Init, PropName <: String, PropType](
      init: Object[F, Init],
      pname: SingletonType[PropName],
      ptype: F[PropType],
    ) extends Object.NonEmpty[F, Init || PropName :? PropType]

    def empty[F[_]]: Object[F, Void] =
      Empty()

    def snoc[F[_], Init, PropType](
      init: SchemaMotif[F, Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Object.NonEmpty[F, Init || pname.type :: PropType] =
      Snoc(asObject(init), SingletonType(pname), ptype)

    def snocOpt[F[_], Init, PropType](
      init: SchemaMotif[F, Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Object.NonEmpty[F, Init || pname.type :? PropType] =
      SnocOpt(asObject(init), SingletonType(pname), ptype)
  }

  def asObject[F[_], Ps](s: SchemaMotif[F, Obj[Ps]]): SchemaMotif.Object[F, Ps] =
    s match
      case o: Object[F, Ps] => o
}
