package jing.openapi.model

import libretto.lambda.Items1Named
import libretto.lambda.Items1Named.Member
import libretto.lambda.util.{SingletonType, TypeEq}
import libretto.lambda.util.TypeEq.Refl

sealed trait Value[T] {
  def show: String =
    val b = new StringBuilder
    show(b)
    b.result()

  private def show(b: StringBuilder): Unit = {
    import Value.*

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
      case Array(elems) =>
        b.append("[")
        for (x <- elems) {
          x.show(b)
          b.append(",")
        }
        b.append("]")
      case o: Object[ps] =>
        b.append("{")
        def go[Ps](o: Object[Ps]): Unit = {
          o match
            case Object.ObjEmpty => // do nothing
            case Object.ObjExt(init, lastName, lastValue) =>
              go(init)
              b.append(lastName: String)
              b.append(": ")
              lastValue.show(b)
              b.append(",")
            case Object.ObjExtOpt(init, lastName, lastValue) =>
              go(init)
              lastValue match
                case None =>
                  // do nothing
                case Some(v) =>
                  b.append(lastName: String)
                  b.append(": ")
                  v.show(b)
                  b.append(",")
        }
        go(o)
        b.append("}")
      case Sum(underlying) =>
        b.append(underlying.label)
        b.append("(")
        underlying.value.show(b)
        b.append(")")
      case Oopsy(message, details) =>
        b.append("Oops()")
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

object Value {
  case object Uno extends Value[Unit]
  case class StringValue(s: String) extends Value[Str]
  case class Int32Value(i: Int) extends Value[Int32]
  case class Int64Value(i: Long) extends Value[Int64]
  case class BoolValue(b: Boolean) extends Value[Bool]
  case class Array[T](elems: IArray[Value[T]]) extends Value[Arr[T]]

  sealed trait Object[Ps] extends Value[Obj[Ps]] {
    def set[T](propName: String, value: Value[T]): Value[Obj[Ps || propName.type :: T]] =
      Object.ObjExt(this, propName, value)

    def set(propName: String, value: String): Value[Obj[Ps || propName.type :: Str]] =
      Object.ObjExt(this, propName, StringValue(value))
  }

  object Object {
    case object ObjEmpty extends Value.Object[{}]
    case class ObjExt[Init, PropName <: String, PropType](
      init: Object[Init],
      lastName: PropName,
      lastValue: Value[PropType],
    ) extends Value.Object[Init || PropName :: PropType]
    case class ObjExtOpt[Init, PropName <: String, PropType](
      init: Object[Init],
      lastName: PropName,
      lastValue: Option[Value[PropType]],
    ) extends Value.Object[Init || PropName :? PropType]

    def empty: Value[Obj[{}]] =
      ObjEmpty

    def extend[Base, K <: String, V](
      base: Value[Obj[Base]],
      k: K,
      v: Value[V],
    ): Value[Obj[Base || K :: V]] =
      ObjExt(asObject(base), k, v)

    def extendOpt[Base, K <: String, V](
      base: Value[Obj[Base]],
      k: K,
      v: Option[Value[V]],
    ): Value[Obj[Base || K :? V]] =
      ObjExtOpt(asObject(base), k, v)
  }

  case class Sum[As](
    value: Items1Named.Sum[||, ::, Value, As]
  ) extends Value[DiscriminatedUnion[As]] {
    def discriminator: DiscriminatorOf[As] =
      discriminatorImpl(value.tag)

    private def discriminatorImpl[Lbl, A, Cases](member: Member[||, ::, Lbl, A, Cases]): DiscriminatorOf[Cases] = {
      import Member.{InInit, InLast, Single}

      member match
        case last: InLast[sep, of, init, lbl, a] =>
          summon[(init || lbl :: a) =:= Cases].substituteCo[DiscriminatorOf](
            last.label.value
          )
        case Single(label) =>
          summon[(Lbl :: A) =:= Cases].substituteCo[DiscriminatorOf](
            label.value: DiscriminatorOf[Lbl :: A]
          )
        case i: InInit[sep, of, lbl, a, init, lblB, b] =>
          summon[(init || lblB :: b) =:= Cases].substituteCo[DiscriminatorOf](
            (discriminatorImpl[lbl, a, init](i.i)
              : DiscriminatorOf[init])
              : DiscriminatorOf[init || lblB :: b]
          )
    }
  }

  case class Oopsy[S <: String](
    message: SingletonType[S],
    details: Option[String],
  ) extends Value[Oops[S]]

  def unit: Value[Unit] = Value.Uno
  def str(s: String): Value[Str] = Value.StringValue(s)
  def int32(i: Int): Value[Int32] = Value.Int32Value(i)
  def int64(i: Long): Value[Int64] = Value.Int64Value(i)
  def bool(b: Boolean): Value[Bool] = Value.BoolValue(b)
  def obj: Value.Object[{}] = Value.Object.ObjEmpty

  def discriminatedUnion[Label <: String, A, As](
    discriminator: Member[||, ::, Label, A, As],
    value: Value[A],
  ): Value[DiscriminatedUnion[As]] =
    Sum(Items1Named.Sum.Value(discriminator, value))

  def oops[S <: String](message: SingletonType[S], details: Option[String]): Value[Oops[S]] =
    Oopsy(message, details)

  def oops(message: String, details: Option[String] = None): Value[Oops[message.type]] =
    oops(SingletonType(message), details)

  def toMap[Ps](value: Value[Obj[Ps]]): Map[String, Value[?]] =
    value match
      case Object.ObjEmpty => Map.empty
      case Object.ObjExt(init, k, v) => toMap(init).updated(k, v)
      case Object.ObjExtOpt(init, k, vOpt) =>
        val m0 = toMap(init)
        vOpt match
          case Some(v) => m0.updated(k, v)
          case None    => m0

  def asObject[Ps](value: Value[Obj[Ps]]): Value.Object[Ps] =
    value match
      case o: Value.Object[ps] => o

  extension [Cases](value: Value[DiscriminatedUnion[Cases]]) {
    private def asSum: Value.Sum[Cases] =
      value match
        case s @ Sum(_) => s

    def discriminator: DiscriminatorOf[Cases] =
      asSum.discriminator

    def assertCase[C <: DiscriminatorOf[Cases]](using ev: C IsCaseOf Cases): Value[ev.Type] =
      val s = value.asSum.value
      assertCaseImpl(IsCaseOf.toMember(ev), s.tag, s.value)
  }

  private def assertCaseImpl[LabelA, A, LabelB, B, Cases](
    expected: Member[||, ::, LabelA, A, Cases],
    actual: Member[||, ::, LabelB, B, Cases],
    value: Value[B],
  ): Value[A] =
    actual.testEqual(expected) match
      case Some(TypeEq(Refl())) =>
        value
      case None =>
        if (expected.label.value == actual.label.value)
          throw IllegalStateException("Seems like you have used the same case label multiple times")
        else
          throw IllegalStateException(s"Expected case: \"${expected.label.value}\", actual case: \"${actual.label.value}\".")

}
