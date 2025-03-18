package jing.openapi.model

import libretto.lambda.util.SingletonValue

sealed trait Schematic[F[_], A]

object Schematic {
  case class I64[F[_]]() extends Schematic[F, Int64]
  case class S[F[_]]() extends Schematic[F, Str]
  case class Array[F[_], T](elem: F[T]) extends Schematic[F, Arr[T]]

  sealed trait Object[F[_], Ps] extends Schematic[F, Obj[Ps]]
  object Object {
    case class Empty[F[_]]() extends Object[F, {}]

    case class Snoc[F[_], Init, PropName <: String, PropType](
      init: F[Obj[Init]],
      pname: SingletonValue[PropName],
      ptype: F[PropType],
    ) extends Object[F, Init || PropName :: PropType] {
      def widen(n: String)(using PropName <:< n.type): Snoc[F, Init, n.type, PropType] =
        Snoc(init, SingletonValue(n), ptype)
    }

    def snoc[F[_], Init, PropType](
      init: F[Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Snoc[F, Init, pname.type, PropType] =
      Snoc(init, SingletonValue(pname), ptype)
  }

  def asObject[F[_], Ps](s: Schematic[F, Obj[Ps]]): Schematic.Object[F, Ps] =
    s match
      case o: Object[F, Ps] => o
}
