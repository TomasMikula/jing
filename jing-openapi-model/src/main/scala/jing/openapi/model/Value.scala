package jing.openapi.model

import libretto.lambda.Items1Named
import libretto.lambda.Items1Named.Member
import libretto.lambda.util.{SingletonType, TypeEq}
import libretto.lambda.util.TypeEq.Refl
import scala.annotation.targetName

enum Value[T] {
  case Proper(underlying: Value.Motif[Value, T])

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

object Value {
  sealed trait Motif[+F[_], T] {
    def show(f: [A] => F[A] => String): String =
      val b = new StringBuilder
      show([A] => (fa, b) => b.append(f(fa)), b)
      b.result()

    def show(
      f: [A] => (F[A], StringBuilder) => Unit,
      b: StringBuilder,
    ): Unit = {
      import Motif.*

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

  object Motif {
    case object Uno extends Motif[Nothing, Unit]
    case class StringValue(s: String) extends Motif[Nothing, Str]
    case class Int32Value(i: Int) extends Motif[Nothing, Int32]
    case class Int64Value(i: Long) extends Motif[Nothing, Int64]
    case class BoolValue(b: Boolean) extends Motif[Nothing, Bool]
    case class Array[F[_], T](elems: IArray[F[T]]) extends Motif[F, Arr[T]]

    sealed trait Object[+F[_], Ps] extends Motif[F, Obj[Ps]] {
      def foreachProperty(
        f: [K <: String, V] => (k: K, v: F[V]) => Unit,
      ): Unit
    }

    object Object {
      case object ObjEmpty extends Motif.Object[Nothing, Void] {
        override def foreachProperty(f: [K <: String, V] => (k: K, v: Nothing) => Unit): Unit =
          () // do nothing
      }

      case class ObjExt[F[_], Init, PropName <: String, PropType](
        init: Object[F, Init],
        lastName: PropName,
        lastValue: F[PropType],
      ) extends Motif.Object[F, Init || PropName :: PropType] {
        override def foreachProperty(f: [K <: String, V] => (k: K, v: F[V]) => Unit): Unit =
          init.foreachProperty(f)
          f(lastName, lastValue)
      }

      case class ObjExtOpt[F[_], Init, PropName <: String, PropType](
        init: Object[F, Init],
        lastName: PropName,
        lastValue: Option[F[PropType]],
      ) extends Motif.Object[F, Init || PropName :? PropType] {
        override def foreachProperty(f: [K <: String, V] => (k: K, v: F[V]) => Unit): Unit =
          init.foreachProperty(f)
          lastValue match
            case None => // do nothing
            case Some(value) => f(lastName, value)
      }

      def empty: Motif[Nothing, Obj[Void]] =
        ObjEmpty

      def extend[F[_], Base, K <: String, V](
        base: Motif[F, Obj[Base]],
        k: K,
        v: F[V],
      ): Motif[F, Obj[Base || K :: V]] =
        ObjExt(asObject(base), k, v)

      def extendOpt[F[_], Base, K <: String, V](
        base: Motif[F, Obj[Base]],
        k: K,
        v: Option[F[V]],
      ): Motif[F, Obj[Base || K :? V]] =
        ObjExtOpt(asObject(base), k, v)

      extension [F[_], Ps](value: Motif[F, Obj[Ps]]) {
        private def asObject: Motif.Object[F, Ps] =
          value match
            case o: Motif.Object[F, ps] => o
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
    ) extends Motif[F, DiscriminatedUnion[As]] {
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
  }

  def unit: Value[Unit] = Proper(Motif.Uno)
  def str(s: String): Value[Str] = Proper(Motif.StringValue(s))
  def int32(i: Int): Value[Int32] = Proper(Motif.Int32Value(i))
  def int64(i: Long): Value[Int64] = Proper(Motif.Int64Value(i))
  def bool(b: Boolean): Value[Bool] = Proper(Motif.BoolValue(b))
  def obj: Value[Obj[Void]] = Proper(Motif.Object.ObjEmpty)
  def arr[T](elems: IArray[Value[T]]): Value[Arr[T]] = Proper(Motif.Array(elems))
  def arr[T](elems: Value[T]*): Value[Arr[T]] = arr(IArray(elems*))

  type ToRightAssoc[Props, Acc] = Props match
    case init || last => ToRightAssoc[init, last || Acc]
    case Void => Acc

  def obj[Props](
    f: ObjectBuilder[Void, ToRightAssoc[Props, Void]] => ObjectBuilder[Props, Void],
  ): Value[Obj[Props]] =
    f(Motif.Object.ObjEmpty).result

  opaque type ObjectBuilder[Acc, Remaining] = Motif.Object[Value, Acc]
  extension [Acc](b: ObjectBuilder[Acc, Void])
    def result: Value[Obj[Acc]] = Proper(b)
  extension [Acc, Label <: String, A, Tail](b: ObjectBuilder[Acc, Label :: A || Tail])
    def set(propName: Label, value: Value[A]): ObjectBuilder[Acc || Label :: A, Tail] =
      Motif.Object.ObjExt(b, propName, value)
  extension [Acc, Label <: String, Tail](b: ObjectBuilder[Acc, Label :: Str || Tail])
    def set(propName: Label, value: String): ObjectBuilder[Acc || Label :: Str, Tail] =
      Motif.Object.ObjExt(b, propName, Value.str(value))

  extension [Acc, Label <: String, A, Tail](b: ObjectBuilder[Acc, Label :? A || Tail]) {
    @targetName("setOpt")
    def set(propName: Label, value: Value[A]): ObjectBuilder[Acc || Label :? A, Tail] =
      Motif.Object.ObjExtOpt(b, propName, Some(value))

    @targetName("setOpt")
    def set(propName: Label, value: String)(using A =:= Str): ObjectBuilder[Acc || Label :? Str, Tail] =
      Motif.Object.ObjExtOpt(b, propName, Some(Value.str(value)))

    @targetName("setOpt")
    def set(propName: Label, value: Long)(using A =:= Int64): ObjectBuilder[Acc || Label :? Int64, Tail] =
      Motif.Object.ObjExtOpt(b, propName, Some(Value.int64(value)))

    def skip(propName: Label): ObjectBuilder[Acc || Label :? A, Tail] =
      Motif.Object.ObjExtOpt(b, propName, None)
  }

  extension [Ps](value: Value[Obj[Ps]]) {
    def set[T](k: String, v: Value[T]): Value[Obj[Ps || k.type :: T]] =
      Proper(Motif.Object.extend(value.asObject, k, v))

    def set(k: String, v: String): Value[Obj[Ps || k.type :: Str]] =
      set(k, str(v))

    def extend[K <: String, V](k: SingletonType[K], v: Value[V]): Value[Obj[Ps || K :: V]] =
      Proper(Motif.Object.extend(value.asObject, k.value, v))

    def extendOpt[K <: String, V](k: SingletonType[K], v: Option[Value[V]]): Value[Obj[Ps || K :? V]] =
      Proper(Motif.Object.extendOpt(value.asObject, k.value, v))

    private def asObject: Motif.Object[Value, Ps] =
      value match
        case Proper(o: Motif.Object[Value, ps]) =>
          o

    def toMap: Map[String, Value[?]] =
      toMapImpl(value.asObject)
  }

  private def toMapImpl[Ps](obj: Motif.Object[Value, Ps]): Map[String, Value[?]] =
    obj match
      case Motif.Object.ObjEmpty => Map.empty
      case Motif.Object.ObjExt(init, k, v) => toMapImpl(init).updated(k, v)
      case Motif.Object.ObjExtOpt(init, k, vOpt) =>
        val m0 = toMapImpl(init)
        vOpt match
          case Some(v) => m0.updated(k, v)
          case None    => m0

  def discriminatedUnion[Label <: String, A, As](
    discriminator: Member[||, ::, Label, A, As],
    value: Value[A],
  ): Value[DiscriminatedUnion[As]] =
    Proper(Motif.DiscUnion(Items1Named.Sum.Value(discriminator, value)))

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
        case Proper(Motif.StringValue(s)) => s

  extension (value: Value[Int64])
    def longValue: Long =
      value match
        case Proper(Motif.Int64Value(n)) => n

  extension (value: Value[Int32])
    def intValue: Int =
      value match
        case Proper(Motif.Int32Value(n)) => n

  extension (value: Value[Bool])
    def booleanValue: Boolean =
      value match
        case Proper(Motif.BoolValue(b)) => b

  extension [T](value: Value[Arr[T]]) {
    def asArray: IArray[Value[T]] =
      value match
        case Proper(Motif.Array(elems)) => elems
  }

  extension [Init, K <: String, V](value: Value[Obj[Init || K :: V]])
    def unsnoc: (Value[Obj[Init]], Value[V]) =
      val (init, v) = value.asObject.unsnoc
      (Proper(init), v)

  extension [Init, K <: String, V](value: Value[Obj[Init || K :? V]])
    @targetName("unsnocOpt")
    def unsnoc: (Value[Obj[Init]], Option[Value[V]]) =
      val (init, v) = value.asObject.unsnoc
      (Proper(init), v)

  extension [Cases](value: Value[DiscriminatedUnion[Cases]]) {
    private def asDiscriminatedUnion: Motif.DiscUnion[Value, Cases] =
      value match
        case Proper(s @ Motif.DiscUnion(_)) => s

    def handleDiscriminatedUnion[R](
      h: [Label <: String, A] => ((Label IsCaseOf Cases) { type Type = A}, Value[A]) => R,
    ): R =
      val v = asDiscriminatedUnion
      h[v.Label, v.ValueType](v.discriminator, v.value)

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
