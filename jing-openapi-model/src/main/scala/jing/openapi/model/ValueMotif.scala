package jing.openapi.model

import libretto.lambda.Items1Named
import libretto.lambda.Items1Named.Member
import scala.annotation.targetName

/** Structure of values, parameterized by nested values `F`. */
sealed trait ValueMotif[+F[_], T] {
  import ValueMotif.*

  def isNotOops[S](using T =:= Oops[S]): Nothing =
    throw AssertionError("Impossible: Values of type Oops[S] are not representable by ValueMotif")

  def sameAs[A](that: ScalaValueOf[A, T]): Boolean =
    that match
      case ScalaValueOf.I32(value) =>
        summon[T =:= Int32]
        summon[A <:< Int]
        this.intValue == value.value
      case ScalaValueOf.I64(value) =>
        summon[T =:= Int64]
        summon[A <:< Long]
        this.longValue == value.value
      case ScalaValueOf.S(value) =>
        summon[T =:= Str]
        summon[A <:< String]
        this.stringValue == value.value
      case ScalaValueOf.B(value) =>
        summon[T =:= Bool]
        summon[A <:< Boolean]
        this.booleanValue == value.value

  def show(f: [A] => F[A] => String): String =
    val b = new StringBuilder
    show([A] => (fa, b) => b.append(f(fa)), b)
    b.result()

  def show(
    f: [A] => (F[A], StringBuilder) => Unit,
    b: StringBuilder,
  ): Unit = {
    import ValueMotif.*

    this match
      case Uno =>
        b.append("()")
      case StringValue(s) =>
        b.append(s)
      case Int32Value(i) =>
        b.append(i.toString)
      case Int64Value(i) =>
        b.append(i.toString)
      case BoolValue(p) =>
        b.append(p.toString)
      case EnumValue(value) =>
        value.show
      case Array(elems) =>
        b.append("[")
        for (x <- elems) {
          f(x, b)
          b.append(",")
        }
        b.append("]")
      case obj: Object[f, ps] =>
        b.append("{")
        def go[Ps](o: Object[F, Ps]): Unit = {
          o match
            case Object.ObjEmpty => // do nothing
            case Object.ObjExt(init, lastName, lastValue) =>
              go(init)
              b.append(lastName: String)
              b.append(": ")
              f(lastValue, b)
              b.append(",")
            case Object.ObjExtOpt(init, lastName, lastValue) =>
              go(init)
              lastValue match
                case None =>
                  // do nothing
                case Some(v) =>
                  b.append(lastName: String)
                  b.append(": ")
                  f(v, b)
                  b.append(",")
        }
        go((obj: Object[f, ps]).asInstanceOf[Object[F, ps]]) // TODO: remove unsafe cast when https://github.com/scala/scala3/issues/22993 is fixed
        b.append("}")
      case DiscUnion(underlying) =>
        b.append(underlying.label)
        b.append("(")
        f(underlying.value, b)
        b.append(")")
  }
}

object ValueMotif {
  case object Uno extends ValueMotif[Nothing, Unit]
  case class StringValue(s: String) extends ValueMotif[Nothing, Str]
  case class Int32Value(i: Int) extends ValueMotif[Nothing, Int32]
  case class Int64Value(i: Long) extends ValueMotif[Nothing, Int64]
  case class BoolValue(b: Boolean) extends ValueMotif[Nothing, Bool]
  case class EnumValue[Base, Cases, T <: ScalaUnionOf[Cases]](value: ScalaValueOf[T, Base]) extends ValueMotif[Nothing, Enum[Base, Cases]]
  case class Array[F[_], T](elems: IArray[F[T]]) extends ValueMotif[F, Arr[T]]

  sealed trait Object[+F[_], Ps] extends ValueMotif[F, Obj[Ps]] {
    def foreachProperty(
      f: [K <: String, V] => (k: K, v: F[V]) => Unit,
    ): Unit
  }

  object Object {
    case object ObjEmpty extends ValueMotif.Object[Nothing, Void] {
      override def foreachProperty(f: [K <: String, V] => (k: K, v: Nothing) => Unit): Unit =
        () // do nothing
    }

    case class ObjExt[F[_], Init, PropName <: String, PropType](
      init: Object[F, Init],
      lastName: PropName,
      lastValue: F[PropType],
    ) extends ValueMotif.Object[F, Init || PropName :: PropType] {
      override def foreachProperty(f: [K <: String, V] => (k: K, v: F[V]) => Unit): Unit =
        init.foreachProperty(f)
        f(lastName, lastValue)
    }

    case class ObjExtOpt[F[_], Init, PropName <: String, PropType](
      init: Object[F, Init],
      lastName: PropName,
      lastValue: Option[F[PropType]],
    ) extends ValueMotif.Object[F, Init || PropName :? PropType] {
      override def foreachProperty(f: [K <: String, V] => (k: K, v: F[V]) => Unit): Unit =
        init.foreachProperty(f)
        lastValue match
          case None => // do nothing
          case Some(value) => f(lastName, value)
    }

    def empty: ValueMotif[Nothing, Obj[Void]] =
      ObjEmpty

    def extend[F[_], Base, K <: String, V](
      base: ValueMotif[F, Obj[Base]],
      k: K,
      v: F[V],
    ): ValueMotif[F, Obj[Base || K :: V]] =
      ObjExt(asObject(base), k, v)

    def extendOpt[F[_], Base, K <: String, V](
      base: ValueMotif[F, Obj[Base]],
      k: K,
      v: Option[F[V]],
    ): ValueMotif[F, Obj[Base || K :? V]] =
      ObjExtOpt(asObject(base), k, v)

    extension [F[_], Ps](value: ValueMotif[F, Obj[Ps]]) {
      private def asObject: ValueMotif.Object[F, Ps] =
        value match
          case o: ValueMotif.Object[F, ps] => o
    }

    extension [F[_], Init, K <: String, V](value: Object[F, Init || K :: V])
      def unsnoc: (Object[F, Init], F[V]) =
        value match
          case Object.ObjExt(init, lastName, lastValue) => (init, lastValue)

    extension [F[_], Init, K <: String, V](value: Object[F, Init || K :? V])
      @targetName("unsnocOpt")
      def unsnoc: (Object[F, Init], Option[F[V]]) =
        value match
          case Object.ObjExtOpt(init, lastName, lastValue) => (init, lastValue)
  }

  case class DiscUnion[F[_], As](
    underlying: Items1Named.Sum[||, ::, F, As],
  ) extends ValueMotif[F, DiscriminatedUnion[As]] {
    opaque type Label <: String = underlying.Label
    opaque type ValueType = underlying.Case

    def discriminator: (Label IsCaseOf As) { type Type = ValueType } =
      IsCaseOf.fromMember(underlying.tag)

    def value: F[ValueType] = underlying.value
    def label: Label = underlying.tag.label.value

    def discriminatorValue: Label & DiscriminatorOf[As] =
      discriminatorValueImpl(underlying.tag)

    private def discriminatorValueImpl[Lbl, A, Cases](member: Member[||, ::, Lbl, A, Cases]): Lbl & DiscriminatorOf[Cases] = {
      import Member.{InInit, InLast, Single}

      member match
        case last: InLast[||, ::, init, lbl, a] =>
          summon[(init || lbl :: a) =:= Cases].substituteCo[[cs] =>> Lbl & DiscriminatorOf[cs]](
            last.label.value
          )
        case Single(label) =>
          summon[(Lbl :: A) =:= Cases].substituteCo[[cs] =>> Lbl & DiscriminatorOf[cs]](
            label.value: DiscriminatorOf[Lbl :: A]
          )
        case i: InInit[||, ::, Lbl, A, init, lblB, b] =>
          summon[(init || lblB :: b) =:= Cases].substituteCo[[cs] =>> Lbl & DiscriminatorOf[cs]](
            (discriminatorValueImpl[Lbl, A, init](i.i)
              : Lbl & DiscriminatorOf[init])
              : Lbl & DiscriminatorOf[init || lblB :: b]
          )
    }
  }

  def discUnion[F[_], Label <: String, As, A](
    i: (Label IsCaseOf As) { type Type = A },
    value: F[A],
  ): ValueMotif[F, DiscriminatedUnion[As]] =
    DiscUnion(Items1Named.Sum.Value(IsCaseOf.toMember(i), value))

  extension [F[_]](value: ValueMotif[F, Str])
    def stringValue: String =
      value match { case StringValue(s) => s }

  extension [F[_]](value: ValueMotif[F, Int32])
    def intValue: Int =
      value match { case Int32Value(i) => i }

  extension [F[_]](value: ValueMotif[F, Int64])
    def longValue: Long =
      value match { case Int64Value(i) => i }

  extension [F[_]](value: ValueMotif[F, Bool])
    def booleanValue: Boolean =
      value match { case BoolValue(b) => b }

  extension [F[_], Base, Cases](value: ValueMotif[F, Enum[Base, Cases]]) {
    def widenEnum: ValueMotif[F, Base] =
      value match
        case ValueMotif.EnumValue(value) =>
          value match
            case ScalaValueOf.I32(i) => Int32Value(i.value)
            case ScalaValueOf.I64(l) => Int64Value(l.value)
            case ScalaValueOf.S(s)   => StringValue(s.value)
            case ScalaValueOf.B(b)   => BoolValue(b.value)

    def extendEnum[A]: ValueMotif[F, Enum[Base, Cases || A]] =
      value match
        case EnumValue(value) =>
          EnumValue(value)
  }
}
