package jing.openapi.model

import libretto.lambda.util.SingletonType

enum Value[T] {
  case Proper(underlying: ValueMotif[Value, T])

  case Oopsy[S <: String](
    message: SingletonType[S],
    details: Option[String],
  ) extends Value[Oops[S]]

  def show: String =
    val b = new StringBuilder
    show(b)
    b.result()

  def show(
    b: StringBuilder,
  ): Unit = {
    this match
      case Proper(underlying) =>
        underlying.show([A] => (va: Value[A], b: StringBuilder) => va.show(b), b)
      case Oopsy(message, details) =>
        b.append("Oops(")
        b.append(message)
        details match
          case Some(s) =>
            b.append(": ")
            b.append(s)
          case None =>
            // do nothing
        b.append(")")
  }
}

object Value extends ValueModule[Value] {
  override def fromMotif[T](v: ValueMotif[Value, T]): Value[T] = Proper(v)

  override def toMotifArr[T](v: Value[Arr[T]]): ValueMotif[Value, Arr[T]]    = v match { case Proper(m) => m }
  override def toMotifBool(v: Value[Bool]): ValueMotif[Value, Bool]          = v match { case Proper(m) => m }
  override def toMotifInt32(v: Value[Int32]): ValueMotif[Value, Int32]       = v match { case Proper(m) => m }
  override def toMotifInt64(v: Value[Int64]): ValueMotif[Value, Int64]       = v match { case Proper(m) => m }
  override def toMotifObj[Ps](v: Value[Obj[Ps]]): ValueMotif[Value, Obj[Ps]] = v match { case Proper(m) => m }
  override def toMotifStr(v: Value[Str]): ValueMotif[Value, Str]             = v match { case Proper(m) => m }
  override def toMotifDiscriminatedUnion[Cases](
    v: Value[DiscriminatedUnion[Cases]]
  ): ValueMotif[Value, DiscriminatedUnion[Cases]] =
    v match { case Proper(m) => m }

  extension [Ps](value: Value[Obj[Ps]]) {
    private def asObject: ValueMotif.Object[Value, Ps] =
      value match
        case Proper(o: ValueMotif.Object[Value, ps]) =>
          o

    def toMap: Map[String, Value[?]] =
      toMapImpl(value.asObject)
  }

  private def toMapImpl[Ps](obj: ValueMotif.Object[Value, Ps]): Map[String, Value[?]] =
    obj match
      case ValueMotif.Object.ObjEmpty => Map.empty
      case ValueMotif.Object.ObjExt(init, k, v) => toMapImpl(init).updated(k, v)
      case ValueMotif.Object.ObjExtOpt(init, k, vOpt) =>
        val m0 = toMapImpl(init)
        vOpt match
          case Some(v) => m0.updated(k, v)
          case None    => m0

  def oops[S <: String](message: SingletonType[S], details: Option[String]): Value[Oops[S]] =
    Oopsy(message, details)

  def oops(message: String, details: Option[String] = None): Value[Oops[message.type]] =
    oops(SingletonType(message), details)

}
