package jing.openapi.model

import libretto.lambda.Items1Named
import libretto.lambda.Items1Named.Member
import libretto.lambda.util.TypeEq.Refl
import libretto.lambda.util.{SingletonType, TypeEq}

import scala.annotation.targetName
import scala.reflect.ClassTag

trait ValueModule[Value[_]] {
  given classTag[T]: ClassTag[Value[T]]

  def fromMotif[T](v: ValueMotif[Value, T]): Value[T]

  def toMotifStr(v: Value[Str]): ValueMotif[Value, Str]
  def toMotifInt32(v: Value[Int32]): ValueMotif[Value, Int32]
  def toMotifInt64(v: Value[Int64]): ValueMotif[Value, Int64]
  def toMotifBool(v: Value[Bool]): ValueMotif[Value, Bool]
  def toMotifEnum[Base, Cases](v: Value[Enum[Base, Cases]]): ValueMotif[Value, Enum[Base, Cases]]
  def toMotifArr[T](v: Value[Arr[T]]): ValueMotif[Value, Arr[T]]
  def toMotifObj[Ps](v: Value[Obj[Ps]]): ValueMotif[Value, Obj[Ps]]
  def toMotifDiscriminatedUnion[Cases](v: Value[DiscriminatedUnion[Cases]]): ValueMotif[Value, DiscriminatedUnion[Cases]]

  /** Deferring the responsibility to keep `Value[A]` distinquishable from `None` to the implementation. */
  def toOption[A](va: Value[A] | None.type): Option[Value[A]]

  /*************************
   ** Constructing values **
   *************************/

  def unit: Value[Unit] = fromMotif(ValueMotif.Uno)
  def str(s: String): Value[Str] = fromMotif(ValueMotif.StringValue(s))
  def int32(i: Int): Value[Int32] = fromMotif(ValueMotif.Int32Value(i))
  def int64(i: Long): Value[Int64] = fromMotif(ValueMotif.Int64Value(i))
  def bool(b: Boolean): Value[Bool] = fromMotif(ValueMotif.BoolValue(b))
  def obj: Value[Obj[Void]] = fromMotif(ValueMotif.Object.empty)
  def arr[T](elems: IArray[Value[T]]): Value[Arr[T]] = fromMotif(ValueMotif.Array(elems))
  def arr[T](elems: Value[T]*): Value[Arr[T]] = arr(IArray(elems*))

  given Conversion[Int, Value[Int32]] =
    int32(_)

  given Conversion[Long, Value[Int64]] =
    int64(_)

  given Conversion[Boolean, Value[Bool]] =
    bool(_)

  given Conversion[String, Value[Str]] =
    str(_)

  def mkEnum[Base, Cases, T <: ScalaUnionOf[Cases]](value: ScalaValueOf[T, Base]): Value[Enum[Base, Cases]] =
    fromMotif(ValueMotif.EnumValue[Base, Cases, T](value))

  def enm[Base, Cases](value: ScalaUnionOf[Cases])(using ev: ScalaValueOf[value.type, Base]): Value[Enum[Base, Cases]] =
    fromMotif(ValueMotif.EnumValue[Base, Cases, value.type](ev))

  @targetName("arrStr")
  def arr(elems: String*): Value[Arr[Str]] =
    arr(elems.map(str)*)

  @targetName("arrInt32")
  def arr(elems: Int*): Value[Arr[Int32]] =
    arr(elems.map(int32)*)

  @targetName("arrInt64")
  def arr(elems: Long*): Value[Arr[Int64]] =
    arr(elems.map(int64)*)

  extension [Ps](value: Value[Obj[Ps]]) {
    def set[T](k: String, v: Value[T]): Value[Obj[Ps || k.type :: T]] =
      extend(SingletonType(k), v)

    def set(k: String, v: String): Value[Obj[Ps || k.type :: Str]] =
      set(k, str(v))

    def setOpt[T](k: String, v: Value[T]): Value[Obj[Ps || k.type :? T]] =
      extendOpt(SingletonType(k), Some(v))

    def setOpt(k: String, v: String): Value[Obj[Ps || k.type :? Str]] =
      setOpt(k, str(v))

    def extend[K <: String, V](k: SingletonType[K], v: Value[V]): Value[Obj[Ps || K :: V]] =
      fromMotif(ValueMotif.Object.extend(toMotifObject(value), k, v))

    def extendOpt[K <: String, V](k: SingletonType[K], v: Option[Value[V]]): Value[Obj[Ps || K :? V]] =
      fromMotif(ValueMotif.Object.extendOpt(toMotifObject(value), k, v))
  }

  opaque type ObjectBuilder[Acc, Remaining] = ValueMotif.Object[Value, Acc]
  object ObjectBuilder {
    def apply[Ps]: ObjectBuilder[Void, ToRightAssoc[Ps]] =
      ValueMotif.Object.empty
  }

  extension [Acc](b: ObjectBuilder[Acc, Void])
    def result: Value[Obj[Acc]] = fromMotif(b)

  extension [Acc, Label <: String, A, Tail](b: ObjectBuilder[Acc, Label :: A || Tail])
    def set(propName: Label, value: Value[A])(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :: A, Tail] =
      ValueMotif.Object.extend(b, l, value)

  extension [Acc, Label <: String, Tail](b: ObjectBuilder[Acc, Label :: Str || Tail])
    def set(propName: Label, value: String)(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :: Str, Tail] =
      ValueMotif.Object.extend(b, l, str(value))

  extension [Acc, Label <: String, Tail](b: ObjectBuilder[Acc, Label :: Int64 || Tail])
    def set(propName: Label, value: Long)(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :: Int64, Tail] =
      ValueMotif.Object.extend(b, l, int64(value))

  extension [Acc, Label <: String, Cases, Tail](b: ObjectBuilder[Acc, Label :: Enum[Str, Cases] || Tail])
    @targetName("setEnum")
    def set(propName: Label, value: ScalaUnionOf[Cases] & String)(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :: Enum[Str, Cases], Tail] =
      ValueMotif.Object.extend(b, l, mkEnum(ScalaValueOf.str(value)))

  extension [Acc, Label <: String, A, Tail](b: ObjectBuilder[Acc, Label :? A || Tail]) {
    @targetName("setOpt")
    def set(propName: Label, value: Value[A])(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :? A, Tail] =
      ValueMotif.Object.extendOpt(b, l, Some(value))

    def setOpt(propName: Label, value: Option[Value[A]])(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :? A, Tail] =
      ValueMotif.Object.extendOpt(b, l, value)

    @targetName("setOpt")
    def set(propName: Label, value: String)(using ev: A =:= Str, l: SingletonType[Label]): ObjectBuilder[Acc || Label :? Str, Tail] =
      ValueMotif.Object.extendOpt(b, l, Some(str(value)))

    @targetName("setOpt")
    def set(propName: Label, value: Long)(using ev: A =:= Int64, l: SingletonType[Label]): ObjectBuilder[Acc || Label :? Int64, Tail] =
      ValueMotif.Object.extendOpt(b, l, Some(int64(value)))

    def skip(propName: Label)(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :? A, Tail] =
      ValueMotif.Object.extendOpt(b, l, None)
  }

  extension [Acc, Label <: String, Cases, Tail](b: ObjectBuilder[Acc, Label :? Enum[Str, Cases] || Tail])
    @targetName("setEnumOpt")
    def set(propName: Label, value: ScalaUnionOf[Cases] & String)(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :? Enum[Str, Cases], Tail] =
      ValueMotif.Object.extendOpt(b, l, Some(mkEnum(ScalaValueOf.str(value))))

  def obj[Props](
    f: ObjectBuilder[Void, ToRightAssoc[Props]] => ObjectBuilder[Props, Void],
  ): Value[Obj[Props]] =
    f(ObjectBuilder[Props]).result

  class ObjectBuilderFromNamedTuple[Props, N <: PropNamesTuple[Props], T <: PropTypesTuple[Value, Props]](
    ps: PropertyList[Props],
  ) {
    def apply(t: NamedTuple.NamedTuple[N, T]): Value[Obj[Props]] =
      fromMotif:
        ValueMotif.Object[Value, Props]:
          ps.readNamedTuple2[Value](t.toTuple)[ValueMotif.Object.Payload[Value]](
            [A] => (va: Value[A]) => va,
            [A] => (va: Value[A] | None.type) => toOption(va),
          )
  }

  def objFromTuple[Props](
    f: ObjectBuilderFromNamedTuple[Props, PropNamesTuple[Props], PropTypesTuple[Value, Props]] => Value[Obj[Props]],
  )(using
    ps: PropertyList[Props],
  ): Value[Obj[Props]] =
    f(ObjectBuilderFromNamedTuple[Props, PropNamesTuple[Props], PropTypesTuple[Value, Props]](ps))

  def discriminatedUnion[Label <: String, A, As](
    discriminator: (Label IsCaseOf As) { type Type = A },
    value: Value[A],
  ): Value[DiscriminatedUnion[As]] =
    fromMotif(ValueMotif.discUnion(discriminator, value))

  class DiscriminatedUnionBuilder[Cases, D <: DiscriminatorOf[Cases]] extends Selectable {
    import DiscriminatedUnionBuilder.*

    type Fields = CaseConstructors[Cases]

    def pick[C <: D & String](using i: C IsCaseOf Cases): PendingValue[Cases, C, i.Type] =
      PendingValue[Cases, C, i.Type](i)

    def selectDynamic(fieldName: D & String): Any =
      (i: IsCaseOf[fieldName.type, Cases]) ?=> (v: Value[i.Type]) =>
        discriminatedUnion[fieldName.type, i.Type, Cases](i, v.asInstanceOf[Value[i.Type]])
  }

  object DiscriminatedUnionBuilder {
    type CaseConstructor[Cases, K <: String] =
      (i: K IsCaseOf Cases) ?=> PendingValue[Cases, K, i.Type]

    type CaseConstructors[Cases] =
      CaseConstructorsAcc[Cases, Cases, NamedTuple.Empty]

    type CaseConstructorsAcc[AllCases, Remaining, Acc] <: NamedTuple.AnyNamedTuple =
      Remaining match
        case k :: _ => NamedTuples.Cons[k, CaseConstructor[AllCases, k], Acc]
        case (init || last) =>
          last match
            case k :: _ => CaseConstructorsAcc[AllCases, init, NamedTuples.Cons[k, CaseConstructor[AllCases, k], Acc]]

    class PendingValue[Cases, K <: String, V](i: (K IsCaseOf Cases) { type Type = V }) {
      def apply(value: Value[V]): Value[DiscriminatedUnion[Cases]] =
        discriminatedUnion(i, value)
    }

  }

  def discriminatedUnion[Cases](
    f: DiscriminatedUnionBuilder[Cases, DiscriminatorOf[Cases]] => Value[DiscriminatedUnion[Cases]],
  ): Value[DiscriminatedUnion[Cases]] =
    f(DiscriminatedUnionBuilder[Cases, DiscriminatorOf[Cases]])


  /***************************
   ** Deconstructing values **
   ***************************/

  extension (value: Value[Str])
    def stringValue: String =
      toMotifStr(value).stringValue

  extension (value: Value[Int64])
    def longValue: Long =
      toMotifInt64(value).longValue

  extension (value: Value[Int32])
    def intValue: Int =
      toMotifInt32(value).intValue

  extension (value: Value[Bool])
    def booleanValue: Boolean =
      toMotifBool(value).booleanValue

  extension [Base, Cases](value: Value[Enum[Base, Cases]]) {
    def widenEnum: Value[Base] =
      fromMotif(toMotifEnum(value).widenEnum)

    def extendEnum[A]: Value[Enum[Base, Cases || A]] =
      fromMotif(toMotifEnum(value).extendEnum)

    def scalaValue: ScalaUnionOf[Cases] =
      toMotifEnum(value).scalaValue
  }

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

  extension [Ps](value: Value[Obj[Ps]]) {
    /** Get the property `K` of this object.
     *
     * Example:
     *
     *     val p: Value[Obj[Void || "x" :: Int32 || "y" :: Int32 || "z" :? Int32]] =
     *       ???
     *
     *     obj.get["x"]: Value[Int32]
     *     obj.get["y"]: Value[Int32]
     *     obj.get["z"]: Option[Value[Int32]]
     *
     * The type argument `K` is constrained to (the union of) property names (`"x" | "y" | "z"`).
     *
     * **For better IDE hints** on the possible values of `K`, try the indirect alternative `obj.props.apply[K]`:
     *
     *     obj.props["x"]: Value[Int32]
     *     obj.props["y"]: Value[Int32]
     *     obj.props["z"]: Option[Value[Int32]]
     *
     * @see [[props]] The indirect alternative with better IDE hints about the actual property names.
     */
    def get[K <: NamesOf[Ps]](using i: IsPropertyOf[K, Ps]): i.Modality[Value[i.Type]] =
      toMotifObject(value).get[K]

    /** Intermediary for accessing `Obj`ect's properties.
     *
     * Methods on the resulting object get better IDE hints than extension methods directly on `Value[Obj[...]]`,
     * likely due to https://github.com/scalameta/metals/issues/7556.
     */
    def props: PropGetter[Ps, NamesOf[Ps]] =
      PropGetter(toMotifObject(value))
  }

  class PropGetter[Ps, KeySet <: NamesOf[Ps]](obj: ValueMotif.Object[Value, Ps]) extends Selectable {
    // TODO: If we had proof of name uniqueness, we could simplify each field's type
    // to be directly what we want (Value[v] or Option[Value[v]]), without taking further given IsPropertyOf witnesses.
    type PropertyGetters[Remaining, Acc] =
      Remaining match
        case Void => Acc
        case init || last =>
          last match
            case k :: v =>
              PropertyGetters[
                init,
                NamedTuples.Cons[k, (ev: (k IsPropertyOf Ps) { type Type = v; type Modality[A] = A }) ?=> Value[v], Acc]
              ]
            case k :? v =>
              PropertyGetters[
                init,
                NamedTuples.Cons[k, (ev: (k IsPropertyOf Ps) { type Type = v; type Modality = Option }) ?=> Option[Value[v]], Acc]
              ]

    type Fields = PropertyGetters[Ps, NamedTuple.Empty]

    def selectDynamic(propName: String): (ev: propName.type IsPropertyOf Ps) ?=> ev.Modality[Value[ev.Type]] =
      (ev: propName.type IsPropertyOf Ps) ?=> obj.get[propName.type]

    /** Get the property `K` of this object.
     *
     * Example:
     *
     *     val p: Value[Obj[Void || "x" :: Int32 || "y" :: Int32 || "z" :? Int32]] =
     *       ???
     *
     *     obj.props["x"]: Value[Int32]
     *     obj.props["y"]: Value[Int32]
     *     obj.props["z"]: Option[Value[Int32]]
     *
     * The type argument `K` is constrained to (the union of) property names (`"x" | "y" | "z"`).
     */
    def apply[K <: KeySet](using i: IsPropertyOf[K, Ps]): i.Modality[Value[i.Type]] =
      obj.get[K]
  }

  extension [Cases](value: Value[DiscriminatedUnion[Cases]]) {
    private def asDiscriminatedUnion: ValueMotif.DiscUnion[? <: Value, Cases] =
      toMotifDiscriminatedUnion(value) match
        case s @ ValueMotif.DiscUnion(_) => s

    def handleDiscriminatedUnion[R](
      h: [Label <: String, A] => ((Label IsCaseOf Cases) { type Type = A}, Value[A]) => R,
    ): R =
      val v = asDiscriminatedUnion
      h[v.Label, v.ValueType](v.discriminator, v.value)

    def discriminator: DiscriminatorOf[Cases] & String =
      asDiscriminatedUnion.discriminatorValue

    def assertCase: AssertCase[Cases, DiscriminatorOf[Cases]] =
      AssertCase(value.asDiscriminatedUnion)

    def switch: SwitchBuilderInit[Cases, NamedCasesToRightAssoc[Cases]] =
      SwitchBuilderInit(value)
  }

  extension [K, V](value: Value[DiscriminatedUnion[K :: V]]) {
    def getUniqueCase: Value[V] =
      value.asDiscriminatedUnion.underlying.getSingle
  }

  class AssertCase[Cases, DiscriminatorSet <: DiscriminatorOf[Cases]](value: ValueMotif.DiscUnion[? <: Value, Cases]) {
    def apply[C <: DiscriminatorSet](using ev: C IsCaseOf Cases): Value[ev.Type] =
      val s = value.underlying
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

  case class SwitchBuilderInit[Cases, CasesRA](value: Value[DiscriminatedUnion[Cases]])

  extension [K, V](b: SwitchBuilderInit[K :: V, K :: V]) {
    def is(k: K)[R](f: Value[V] => R): R =
      f(b.value.getUniqueCase)
  }

  extension [Cases, K <: String, V, Tail](b: SwitchBuilderInit[Cases, K :: V || Tail]) {
    def is(k: K)(using SingletonType[K])[R](f: Value[V] => R): SwitchBuilder[Cases, K :: V, Tail, R] =
      SwitchBuilder(b.value, Items1Named.Product.Single(summon[SingletonType[K]], f))
  }

  case class SwitchBuilder[Cases, Acc, Tail, R](
    value: Value[DiscriminatedUnion[Cases]],
    handlerAcc: Items1Named.Product[||, ::, [v] =>> Value[v] => R, Acc],
  )

  extension [Cases, Acc, K <: String, V, Tail, R](b: SwitchBuilder[Cases, Acc, K :: V || Tail, R]) {
    def is(k: K)(using SingletonType[K])(f: Value[V] => R): SwitchBuilder[Cases, Acc || K :: V, Tail, R] =
      SwitchBuilder(b.value, Items1Named.Product.Snoc(b.handlerAcc, summon[SingletonType[K]], f))
  }

  extension [Cases, Acc, K <: String, V, R](b: SwitchBuilder[Cases, Acc, K :: V, R]) {
    def is(k: K)(using SingletonType[K])(f: Value[V] => R): SwitchBuilderDone[Cases, Acc || K :: V, R] =
      SwitchBuilderDone(b.value, Items1Named.Product.Snoc(b.handlerAcc, summon[SingletonType[K]], f))
  }

  case class SwitchBuilderDone[Cases, Acc, R](
    value: Value[DiscriminatedUnion[Cases]],
    handlerAcc: Items1Named.Product[||, ::, [v] =>> Value[v] => R, Acc],
  ) {
    def end(using ev: Acc =:= Cases): R =
      val x = ev.substituteContra(value.asDiscriminatedUnion.underlying)
      handlerAcc
        .get(x.tag)
        .apply(x.value)
  }
}
