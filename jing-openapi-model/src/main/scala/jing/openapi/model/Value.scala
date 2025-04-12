package jing.openapi.model

import libretto.lambda.Items1Named
import libretto.lambda.Items1Named.Member
import libretto.lambda.util.{SingletonType, TypeEq}
import libretto.lambda.util.TypeEq.Refl
import scala.annotation.targetName

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
      case DiscUnion(underlying) =>
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

  sealed trait Object[Ps] extends Value[Obj[Ps]]

  object Object {
    case object ObjEmpty extends Value.Object[Void]
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

    def empty: Value[Obj[Void]] =
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

  case class DiscUnion[As](
    underlying: Items1Named.Sum[||, ::, Value, As],
  ) extends Value[DiscriminatedUnion[As]] {
    opaque type Label <: String = underlying.Label
    opaque type ValueType = underlying.Case

    def discriminator: (Label IsCaseOf As) { type Type = ValueType } =
      IsCaseOf.fromMember(underlying.tag)

    def value: Value[ValueType] = underlying.value
    def label: Label = underlying.tag.label.value

    def discriminatorValue: Label & DiscriminatorOf[As] =
      discriminatorValueImpl(underlying.tag)

    private def discriminatorValueImpl[Lbl, A, Cases](member: Member[||, ::, Lbl, A, Cases]): Lbl & DiscriminatorOf[Cases] = {
      import Member.{InInit, InLast, Single}

      member match
        case last: InLast[sep, of, init, lbl, a] =>
          summon[(init || lbl :: a) =:= Cases].substituteCo[[cs] =>> Lbl & DiscriminatorOf[cs]](
            last.label.value
          )
        case Single(label) =>
          summon[(Lbl :: A) =:= Cases].substituteCo[[cs] =>> Lbl & DiscriminatorOf[cs]](
            label.value: DiscriminatorOf[Lbl :: A]
          )
        case i: InInit[sep, of, lbl, a, init, lblB, b] =>
          summon[(init || lblB :: b) =:= Cases].substituteCo[[cs] =>> Lbl & DiscriminatorOf[cs]](
            (discriminatorValueImpl[lbl, a, init](i.i)
              : Lbl & DiscriminatorOf[init])
              : Lbl & DiscriminatorOf[init || lblB :: b]
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
  def obj: Value[Obj[Void]] = Value.Object.ObjEmpty
  def arr[T](elems: Value[T]*): Value[Arr[T]] = Value.Array(IArray(elems*))

  type ToRightAssoc[Props, Acc] = Props match
    case init || last => ToRightAssoc[init, last || Acc]
    case Void => Acc

  def obj[Props](
    f: ObjectBuilder[Void, ToRightAssoc[Props, Void]] => ObjectBuilder[Props, Void],
  ): Value[Obj[Props]] =
    f(Object.ObjEmpty).result

  opaque type ObjectBuilder[Acc, Remaining] = Value.Object[Acc]
  extension [Acc](b: ObjectBuilder[Acc, {}])
    def result: Value[Obj[Acc]] = b
  extension [Acc, Label <: String, A, Tail](b: ObjectBuilder[Acc, Label :: A || Tail])
    def set(propName: Label, value: Value[A]): ObjectBuilder[Acc || Label :: A, Tail] =
      Object.ObjExt[Acc, Label, A](b, propName, value)
  extension [Acc, Label <: String, Tail](b: ObjectBuilder[Acc, Label :: Str || Tail])
    def set(propName: Label, value: String): ObjectBuilder[Acc || Label :: Str, Tail] =
      Object.ObjExt[Acc, Label, Str](b, propName, Value.str(value))

  extension [Acc, Label <: String, A, Tail](b: ObjectBuilder[Acc, Label :? A || Tail]) {
    @targetName("setOpt")
    def set(propName: Label, value: Value[A]): ObjectBuilder[Acc || Label :? A, Tail] =
      Object.ObjExtOpt[Acc, Label, A](b, propName, Some(value))

    @targetName("setOpt")
    def set(propName: Label, value: String)(using A =:= Str): ObjectBuilder[Acc || Label :? Str, Tail] =
      Object.ObjExtOpt[Acc, Label, Str](b, propName, Some(Value.str(value)))

    @targetName("setOpt")
    def set(propName: Label, value: Long)(using A =:= Int64): ObjectBuilder[Acc || Label :? Int64, Tail] =
      Object.ObjExtOpt[Acc, Label, Int64](b, propName, Some(Value.int64(value)))

    def skip(propName: Label): ObjectBuilder[Acc || Label :? A, Tail] =
      Object.ObjExtOpt[Acc, Label, A](b, propName, None)
  }

  def discriminatedUnion[Label <: String, A, As](
    discriminator: Member[||, ::, Label, A, As],
    value: Value[A],
  ): Value[DiscriminatedUnion[As]] =
    DiscUnion(Items1Named.Sum.Value(discriminator, value))

  def discriminatedUnion[Cases](
    f: DiscriminatedUnionBuilder[Cases] => Value[DiscriminatedUnion[Cases]],
  ): Value[DiscriminatedUnion[Cases]] =
    f(())

  opaque type DiscriminatedUnionBuilder[Cases] = Unit
  extension [Cases](b: DiscriminatedUnionBuilder[Cases]) {
    def pick[C <: String](using i: C IsCaseOf Cases)(value: Value[i.Type]): Value[DiscriminatedUnion[Cases]] =
      discriminatedUnion(IsCaseOf.toMember(i), value)
  }

  def oops[S <: String](message: SingletonType[S], details: Option[String]): Value[Oops[S]] =
    Oopsy(message, details)

  def oops(message: String, details: Option[String] = None): Value[Oops[message.type]] =
    oops(SingletonType(message), details)

  extension (value: Value[Str])
    def stringValue: String =
      value match
        case StringValue(s) => s

  extension (value: Value[Int64])
    def longValue: Long =
      value match
        case Int64Value(n) => n

  extension (value: Value[Int32])
    def intValue: Int =
      value match
        case Int32Value(n) => n

  extension (value: Value[Bool])
    def booleanValue: Boolean =
      value match
        case BoolValue(b) => b

  extension [T](value: Value[Arr[T]]) {
    def asArray: IArray[Value[T]] =
      value match
        case Value.Array(elems) => elems
  }

  extension [Ps](value: Value[Obj[Ps]]) {
    def set[T](k: String, v: Value[T]): Value[Obj[Ps || k.type :: T]] =
      Object.ObjExt(value.asObject, k, v)

    def set(k: String, v: String): Value[Obj[Ps || k.type :: Str]] =
      Object.ObjExt(value.asObject, k, StringValue(v))

    private def asObject: Value.Object[Ps] =
      value match
        case o: Value.Object[ps] => o

    def toMap: Map[String, Value[?]] =
      value match
        case Object.ObjEmpty => Map.empty
        case Object.ObjExt(init, k, v) => init.toMap.updated(k, v)
        case Object.ObjExtOpt(init, k, vOpt) =>
          val m0 = init.toMap
          vOpt match
            case Some(v) => m0.updated(k, v)
            case None    => m0
  }

  extension [Init, K <: String, V](value: Value[Obj[Init || K :: V]])
    def unsnoc: (Value[Obj[Init]], Value[V]) =
      value match
        case Object.ObjExt(init, lastName, lastValue) => (init, lastValue)

  extension [Init, K <: String, V](value: Value[Obj[Init || K :? V]])
    @targetName("unsnocOpt")
    def unsnoc: (Value[Obj[Init]], Option[Value[V]]) =
      value match
        case Object.ObjExtOpt(init, lastName, lastValue) => (init, lastValue)

  extension [Cases](value: Value[DiscriminatedUnion[Cases]]) {
    def asDiscriminatedUnion: Value.DiscUnion[Cases] =
      value match
        case s @ DiscUnion(_) => s

    def discriminator: DiscriminatorOf[Cases] =
      asDiscriminatedUnion.discriminatorValue

    def assertCase[C <: DiscriminatorOf[Cases]](using ev: C IsCaseOf Cases): Value[ev.Type] =
      val s = value.asDiscriminatedUnion.underlying
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
