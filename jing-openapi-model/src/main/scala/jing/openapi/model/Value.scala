package jing.openapi.model

import libretto.lambda.util.{SingletonType, Validated}

import scala.reflect.ClassTag

case class Value[T](underlying: ValueMotif[Value, T]) {
  def show: String =
    Value.show(this)

  def isNotOops[S](using T =:= Oops[S]): Nothing =
    underlying.isNotOops[S]

  override def toString: String =
    show
}

object Value extends ValueModule[Value] {
  given ValueModule[Value] = this

  /** Value permitting nested errors ([[Oops]]-typed values). */
  enum Lenient[T] {
    case Proper(underlying: ValueMotif[Value.Lenient, T])

    case Oopsy[S <: String](
      message: SingletonType[S],
      details: Option[String],
    ) extends Value.Lenient[Oops[S]]

    def show: String =
      Lenient.show(this)

    def toValue: Validated[Oopsy[? <: String], Value[T]] =
      this match
        case Proper(underlying) =>
          underlying
            .traverse[Validated[Oopsy[? <: String], _], Value]([A] => lva => lva.toValue)
            .map(Value(_))
        case o @ Oopsy(_, _) =>
          Validated.invalid(o)

    override def toString: String =
      show
  }

  object Lenient extends ValueModule[Lenient] {
    given ValueModule[Lenient] = this

    override def classTag[T]: ClassTag[Lenient[T]] = summon[ClassTag[Lenient[T]]]

    override def fromMotif[T](v: ValueMotif[Lenient, T]): Lenient[T] = Proper(v)

    override def toMotifArr[T](v: Lenient[Arr[T]]): ValueMotif[Lenient, Arr[T]]    = v match { case Proper(m) => m }
    override def toMotifBool(v: Lenient[Bool]): ValueMotif[Lenient, Bool]          = v match { case Proper(m) => m }
    override def toMotifInt32(v: Lenient[Int32]): ValueMotif[Lenient, Int32]       = v match { case Proper(m) => m }
    override def toMotifInt64(v: Lenient[Int64]): ValueMotif[Lenient, Int64]       = v match { case Proper(m) => m }
    override def toMotifObj[Ps](v: Lenient[Obj[Ps]]): ValueMotif[Lenient, Obj[Ps]] = v match { case Proper(m) => m }
    override def toMotifStr(v: Lenient[Str]): ValueMotif[Lenient, Str]             = v match { case Proper(m) => m }

    override def toMotifEnum[Base, Cases](v: Lenient[Enum[Base, Cases]]): ValueMotif[Lenient, Enum[Base, Cases]] =
      v match { case Proper(m) => m }

    override def toMotifDiscriminatedUnion[Cases](
      v: Lenient[DiscriminatedUnion[Cases]]
    ): ValueMotif[Lenient, DiscriminatedUnion[Cases]] =
      v match { case Proper(m) => m }

    override def toOption[A](va: Lenient[A] | None.type): Option[Lenient[A]] =
      va match
        case va: Lenient[A] => Some(va)
        case None => None

    override def showAppend[A](v: Lenient[A])(b: StringBuilder): Unit = {
      v match
        case Proper(underlying) =>
          underlying.show([A] => (va: Lenient[A], c: StringBuilder) => showAppend(va)(c), b)
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

    def oops[S <: String](message: SingletonType[S], details: Option[String]): Lenient[Oops[S]] =
      Oopsy(message, details)

    def oops(message: String, details: Option[String] = None): Lenient[Oops[message.type]] =
      oops(SingletonType(message), details)
  }

  override def classTag[T]: ClassTag[Value[T]] = summon[ClassTag[Value[T]]]

  override def fromMotif[T](v: ValueMotif[Value, T]): Value[T] = Value(v)

  override def toMotifArr[T](v: Value[Arr[T]]): ValueMotif[Value, Arr[T]]    = v.underlying
  override def toMotifBool(v: Value[Bool]): ValueMotif[Value, Bool]          = v.underlying
  override def toMotifInt32(v: Value[Int32]): ValueMotif[Value, Int32]       = v.underlying
  override def toMotifInt64(v: Value[Int64]): ValueMotif[Value, Int64]       = v.underlying
  override def toMotifObj[Ps](v: Value[Obj[Ps]]): ValueMotif[Value, Obj[Ps]] = v.underlying
  override def toMotifStr(v: Value[Str]): ValueMotif[Value, Str]             = v.underlying

  override def toMotifEnum[Base, Cases](v: Value[Enum[Base, Cases]]): ValueMotif[Value, Enum[Base, Cases]] =
    v.underlying

  override def toMotifDiscriminatedUnion[Cases](
    v: Value[DiscriminatedUnion[Cases]]
  ): ValueMotif[Value, DiscriminatedUnion[Cases]] =
    v.underlying

  override def toOption[A](va: Value[A] | None.type): Option[Value[A]] =
    va match
      case va: Value[A] => Some(va)
      case None => None

  override def showAppend[A](v: Value[A])(b: StringBuilder): Unit =
    v.underlying.show([X] => (vx: Value[X], c: StringBuilder) => showAppend(vx)(c), b)
}
