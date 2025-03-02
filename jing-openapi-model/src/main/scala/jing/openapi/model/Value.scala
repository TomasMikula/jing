package jing.openapi.model

sealed trait Value[T]

object Value {
  case class StringValue(s: String) extends Value[Str]
  case class Int64Value(i: Long) extends Value[Int64]

  sealed trait Object[Ps] extends Value[Obj[Ps]] {
    def set[T](propName: String, value: Value[T]): Value[Obj[Ps || propName.type :: T]] =
      Object.ObjExt(this, propName, value)

    def set(propName: String, value: String): Value[Obj[Ps || propName.type :: Str]] =
      Object.ObjExt(this, propName, StringValue(value))
  }

  object Object {
    case object ObjEmpty extends Value.Object[{}]
    case class ObjExt[Init, PropName <: String, PropType](
      init: Value[Obj[Init]],
      lastName: PropName,
      lastValue: Value[PropType],
    ) extends Value.Object[Init || PropName :: PropType]
  }

  // case Pair[A, B](a: Value[A], b: Value[B]) extends Value[A ** B]

  // def **[U](that: Value[U]): Value[T ** U] = Pair(this, that)

  def str(s: String): Value[Str] = Value.StringValue(s)
  def int64(i: Long): Value[Int64] = Value.Int64Value(i)
  def obj: Value.Object[{}] = Value.Object.ObjEmpty

  def toMap[Ps](value: Value[Obj[Ps]]): Map[String, Value[?]] =
    value match
      case Object.ObjEmpty => Map.empty
      case Object.ObjExt(init, k, v) => toMap(init).updated(k, v)

}
