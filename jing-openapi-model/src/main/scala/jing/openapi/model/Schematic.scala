package jing.openapi.model

import libretto.lambda.util.{Applicative, Exists, SingletonValue}

/** Schema structure parametric in the type of nested schemas.
 *
 * @tparam F representation of nested schemas. In [[Schema]],
 *   [[Schematic]] is used with `F` recursively instantiated to [[Schema]] itself.
 */
sealed trait Schematic[F[_], A] {
  import Schematic.*

  def translate[G[_]](h: [X] => F[X] => G[X]): Schematic[G, A] =
    this match
      case I64() => I64()
      case S() => S()
      case Array(elem) => Array(h(elem))
      case Object.Empty() => Object.Empty()
      case Object.Snoc(init, pname, ptype) => Object.Snoc(asObject(init.translate(h)), pname, h(ptype))

  def wipeTranslate[G[_]](h: [X] => F[X] => Exists[G]): Schematic[G, ?] =
    this match
      case I64() => I64()
      case S() => S()
      case Array(elem) => Array(h(elem).value)
      case o: Object[f, ps] => o.wipeTranslateObj(h).value

  def wipeTranslateA[G[_], H[_]](h: [X] => F[X] => G[Exists[H]])(using G: Applicative[G]): G[Schematic[H, ?]] =
    this match
      case I64() => G.pure(I64())
      case S() => G.pure(S())
      case Array(elem) => h(elem).map(el => Array(el.value))
      case o: Object[f, ps] => o.wipeTranslateObjA(h).map(_.value)

}

object Schematic {
  case class I64[F[_]]() extends Schematic[F, Int64]
  case class S[F[_]]() extends Schematic[F, Str]
  case class Array[F[_], T](elem: F[T]) extends Schematic[F, Arr[T]]

  sealed trait Object[F[_], Ps] extends Schematic[F, Obj[Ps]] {
    private[Schematic] def wipeTranslateObj[G[_]](h: [X] => F[X] => Exists[G]): Exists[[X] =>> Schematic[G, Obj[X]]] =
      this match
        case Object.Empty() =>
          Exists(Object.Empty())
        case Object.Snoc(init, pname, ptype) =>
          Exists(Object.Snoc(asObject(init.wipeTranslateObj(h).value), pname, h(ptype).value))

    private[Schematic] def wipeTranslateObjA[G[_], H[_]](
      h: [X] => F[X] => G[Exists[H]],
    )(using
      G: Applicative[G],
    ): G[Exists[[X] =>> Schematic[H, Obj[X]]]] =
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
  }
  object Object {
    case class Empty[F[_]]() extends Object[F, {}]

    case class Snoc[F[_], Init, PropName <: String, PropType](
      init: Object[F, Init],
      pname: SingletonValue[PropName],
      ptype: F[PropType],
    ) extends Object[F, Init || PropName :: PropType] {
      def widen(n: String)(using PropName <:< n.type): Snoc[F, Init, n.type, PropType] =
        Snoc(init, SingletonValue(n), ptype)
    }

    def snoc[F[_], Init, PropType](
      init: Schematic[F, Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Snoc[F, Init, pname.type, PropType] =
      Snoc(asObject(init), SingletonValue(pname), ptype)
  }

  def asObject[F[_], Ps](s: Schematic[F, Obj[Ps]]): Schematic.Object[F, Ps] =
    s match
      case o: Object[F, Ps] => o
}
