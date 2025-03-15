package jing.openapi.model

import libretto.lambda.util.SingletonValue

sealed trait Schema[A]

object Schema {
  case object I64 extends Schema[Int64]
  case object S extends Schema[Str]
  case class Array[T](elem: Schema[T]) extends Schema[Arr[T]]

  case class Unknown[S <: String](
    reason: SingletonValue[S],
  ) extends Schema[Oops[S]]

  def unknown(reason: String): Unknown[reason.type] =
    Unknown(SingletonValue(reason))

  sealed trait Object[Ps] extends Schema[Obj[Ps]]
  object Object {
    case object Empty extends Object[{}]

    case class Snoc[Init, PropName <: String, PropType](
      init: Schema[Obj[Init]],
      pname: PropName,
      ptype: Schema[PropType],
    )(using
      val singletonPropName: PropName =:= pname.type
    ) extends Object[Init || PropName :: PropType] {
      def widen(n: String)(using PropName <:< n.type): Snoc[Init, n.type, PropType] =
        Snoc(init, n, ptype)
    }

    def snoc[Init, PropType](
      init: Schema[Obj[Init]],
      pname: String,
      ptype: Schema[PropType],
    ): Snoc[Init, pname.type, PropType] =
      Snoc(init, pname, ptype)
  }
}
