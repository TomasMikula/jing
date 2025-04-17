package jing.openapi.model

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
      case I32() => I32()
      case I64() => I64()
      case S() => S()
      case B() => B()
      case Array(elem) => Array(h(elem))
      case Object.Empty() => Object.Empty()
      case Object.Snoc(init, pname, ptype) => Object.Snoc(asObject(init.translate(h)), pname, h(ptype))

  def wipeTranslate[G[_]](h: [X] => F[X] => Exists[G]): SchemaMotif[G, ?] =
    this match
      case I32() => I32()
      case I64() => I64()
      case S() => S()
      case B() => B()
      case Array(elem) => Array(h(elem).value)
      case o: Object[f, ps] => o.wipeTranslateObj(h).value

  def wipeTranslateA[G[_], H[_]](h: [X] => F[X] => G[Exists[H]])(using G: Applicative[G]): G[SchemaMotif[H, ?]] =
    this match
      case I32() => G.pure(I32())
      case I64() => G.pure(I64())
      case S() => G.pure(S())
      case B() => G.pure(B())
      case Array(elem) => h(elem).map(el => Array(el.value))
      case o: Object[f, ps] => o.wipeTranslateObjA(h).map(_.value)

}

object SchemaMotif {
  case class I32[F[_]]() extends SchemaMotif[F, Int32]
  case class I64[F[_]]() extends SchemaMotif[F, Int64]
  case class S[F[_]]() extends SchemaMotif[F, Str]
  case class B[F[_]]() extends SchemaMotif[F, Bool]
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

    case class Snoc[F[_], Init, PropName <: String, PropType](
      init: Object[F, Init],
      pname: SingletonType[PropName],
      ptype: F[PropType],
    ) extends Object[F, Init || PropName :: PropType]

    case class SnocOpt[F[_], Init, PropName <: String, PropType](
      init: Object[F, Init],
      pname: SingletonType[PropName],
      ptype: F[PropType],
    ) extends Object[F, Init || PropName :? PropType]

    def empty[F[_]]: Object[F, Void] =
      Empty()

    def snoc[F[_], Init, PropType](
      init: SchemaMotif[F, Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Object[F, Init || pname.type :: PropType] =
      Snoc(asObject(init), SingletonType(pname), ptype)

    def snocOpt[F[_], Init, PropType](
      init: SchemaMotif[F, Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Object[F, Init || pname.type :? PropType] =
      SnocOpt(asObject(init), SingletonType(pname), ptype)
  }

  def asObject[F[_], Ps](s: SchemaMotif[F, Obj[Ps]]): SchemaMotif.Object[F, Ps] =
    s match
      case o: Object[F, Ps] => o
}
