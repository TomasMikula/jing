package jing.openapi.model

import libretto.lambda.Items1Named.Member
import libretto.lambda.util.{SingletonType, TypeEq}
import libretto.lambda.util.TypeEq.Refl
import scala.annotation.targetName
import scala.reflect.ClassTag

trait ValueModule[Value[_]] {
  def fromMotif[T](v: ValueMotif[Value, T]): Value[T]

  def toMotifStr(v: Value[Str]): ValueMotif[Value, Str]
  def toMotifInt32(v: Value[Int32]): ValueMotif[Value, Int32]
  def toMotifInt64(v: Value[Int64]): ValueMotif[Value, Int64]
  def toMotifBool(v: Value[Bool]): ValueMotif[Value, Bool]
  def toMotifArr[T](v: Value[Arr[T]]): ValueMotif[Value, Arr[T]]
  def toMotifObj[Ps](v: Value[Obj[Ps]]): ValueMotif[Value, Obj[Ps]]
  def toMotifDiscriminatedUnion[Cases](v: Value[DiscriminatedUnion[Cases]]): ValueMotif[Value, DiscriminatedUnion[Cases]]

  /*************************
   ** Constructing values **
   *************************/

  def unit: Value[Unit] = fromMotif(ValueMotif.Uno)
  def str(s: String): Value[Str] = fromMotif(ValueMotif.StringValue(s))
  def int32(i: Int): Value[Int32] = fromMotif(ValueMotif.Int32Value(i))
  def int64(i: Long): Value[Int64] = fromMotif(ValueMotif.Int64Value(i))
  def bool(b: Boolean): Value[Bool] = fromMotif(ValueMotif.BoolValue(b))
  def obj: Value[Obj[Void]] = fromMotif(ValueMotif.Object.ObjEmpty)
  def arr[T](elems: IArray[Value[T]]): Value[Arr[T]] = fromMotif(ValueMotif.Array(elems))
  def arr[T](elems: Value[T]*)(using ClassTag[Value[T]]): Value[Arr[T]] = arr(IArray(elems*))

  extension [Ps](value: Value[Obj[Ps]]) {
    def set[T](k: String, v: Value[T]): Value[Obj[Ps || k.type :: T]] =
      fromMotif(ValueMotif.Object.extend(toMotifObject(value), k, v))

    def set(k: String, v: String): Value[Obj[Ps || k.type :: Str]] =
      set(k, str(v))

    def extend[K <: String, V](k: SingletonType[K], v: Value[V]): Value[Obj[Ps || K :: V]] =
      fromMotif(ValueMotif.Object.extend(toMotifObject(value), k.value, v))

    def extendOpt[K <: String, V](k: SingletonType[K], v: Option[Value[V]]): Value[Obj[Ps || K :? V]] =
      fromMotif(ValueMotif.Object.extendOpt(toMotifObject(value), k.value, v))
  }

  opaque type ObjectBuilder[Acc, Remaining] = ValueMotif.Object[Value, Acc]
  object ObjectBuilder {
    def apply[Ps]: ObjectBuilder[Void, ToRightAssoc[Ps]] =
      ValueMotif.Object.ObjEmpty
  }

  extension [Acc](b: ObjectBuilder[Acc, Void])
    def result: Value[Obj[Acc]] = fromMotif(b)

  extension [Acc, Label <: String, A, Tail](b: ObjectBuilder[Acc, Label :: A || Tail])
    def set(propName: Label, value: Value[A]): ObjectBuilder[Acc || Label :: A, Tail] =
      ValueMotif.Object.ObjExt(b, propName, value)

  extension [Acc, Label <: String, Tail](b: ObjectBuilder[Acc, Label :: Str || Tail])
    def set(propName: Label, value: String): ObjectBuilder[Acc || Label :: Str, Tail] =
      ValueMotif.Object.ObjExt(b, propName, str(value))

  extension [Acc, Label <: String, A, Tail](b: ObjectBuilder[Acc, Label :? A || Tail]) {
    @targetName("setOpt")
    def set(propName: Label, value: Value[A]): ObjectBuilder[Acc || Label :? A, Tail] =
      ValueMotif.Object.ObjExtOpt(b, propName, Some(value))

    @targetName("setOpt")
    def set(propName: Label, value: String)(using A =:= Str): ObjectBuilder[Acc || Label :? Str, Tail] =
      ValueMotif.Object.ObjExtOpt(b, propName, Some(str(value)))

    @targetName("setOpt")
    def set(propName: Label, value: Long)(using A =:= Int64): ObjectBuilder[Acc || Label :? Int64, Tail] =
      ValueMotif.Object.ObjExtOpt(b, propName, Some(int64(value)))

    def skip(propName: Label): ObjectBuilder[Acc || Label :? A, Tail] =
      ValueMotif.Object.ObjExtOpt(b, propName, None)
  }

  def obj[Props](
    f: ObjectBuilder[Void, ToRightAssoc[Props]] => ObjectBuilder[Props, Void],
  ): Value[Obj[Props]] =
    f(ObjectBuilder[Props]).result

  def discriminatedUnion[Label <: String, A, As](
    discriminator: (Label IsCaseOf As) { type Type = A },
    value: Value[A],
  ): Value[DiscriminatedUnion[As]] =
    fromMotif(ValueMotif.discUnion(discriminator, value))

  opaque type DiscriminatedUnionBuilder[Cases] = Unit

  extension [Cases](b: DiscriminatedUnionBuilder[Cases]) {
    def pick[C <: String](using i: C IsCaseOf Cases)(value: Value[i.Type]): Value[DiscriminatedUnion[Cases]] =
      discriminatedUnion(i, value)
  }

  def discriminatedUnion[Cases](
    f: DiscriminatedUnionBuilder[Cases] => Value[DiscriminatedUnion[Cases]],
  ): Value[DiscriminatedUnion[Cases]] =
    f(())


  /***************************
   ** Deconstructing values **
   ***************************/

  extension (value: Value[Str])
    def stringValue: String =
      toMotifStr(value) match
        case ValueMotif.StringValue(s) => s

  extension (value: Value[Int64])
    def longValue: Long =
      toMotifInt64(value) match
        case ValueMotif.Int64Value(n) => n

  extension (value: Value[Int32])
    def intValue: Int =
      toMotifInt32(value) match
        case ValueMotif.Int32Value(n) => n

  extension (value: Value[Bool])
    def booleanValue: Boolean =
      toMotifBool(value) match
        case ValueMotif.BoolValue(b) => b

  extension [T](value: Value[Arr[T]]) {
    def asArray: IArray[Value[T]] =
      toMotifArr(value) match
        case ValueMotif.Array(elems) => elems
  }

  private def toMotifObject[Ps](v: Value[Obj[Ps]]): ValueMotif.Object[Value, Ps] =
    toMotifObj(v) match
      case o: ValueMotif.Object[Value, ps] => o

  extension [Init, K <: String, V](value: Value[Obj[Init || K :: V]])
    def unsnoc: (Value[Obj[Init]], Value[V]) =
      val (init, v) = toMotifObject(value).unsnoc
      (fromMotif(init), v)

  extension [Init, K <: String, V](value: Value[Obj[Init || K :? V]])
    @targetName("unsnocOpt")
    def unsnoc: (Value[Obj[Init]], Option[Value[V]]) =
      val (init, v) = toMotifObject(value).unsnoc
      (fromMotif(init), v)

  extension [Cases](value: Value[DiscriminatedUnion[Cases]]) {
    private def asDiscriminatedUnion: ValueMotif.DiscUnion[? <: Value, Cases] =
      toMotifDiscriminatedUnion(value) match
        case s @ ValueMotif.DiscUnion(_) => s

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
