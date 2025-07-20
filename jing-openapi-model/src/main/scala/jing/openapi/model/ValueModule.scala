package jing.openapi.model

import libretto.lambda.Items1Named
import libretto.lambda.Items1Named.Member
import libretto.lambda.util.TypeEq.Refl
import libretto.lambda.util.{SingletonType, TypeEq}

import scala.NamedTuple.NamedTuple
import scala.annotation.targetName
import scala.reflect.ClassTag

trait ValueModule[Value[_]] {
  /** Needed to construct arrays of `Value[T]`. */
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

  def showAppend[A](v: Value[A])(b: StringBuilder): Unit

  /** Deferring the responsibility to keep `Value[A]` distinquishable from `None` to the implementation. */
  def toOption[A](va: Value[A] | None.type): Option[Value[A]]

  /*************************
   ** Constructing values **
   *************************/

  def unit: Value[Unit] = fromMotif(ValueMotif.Uno)

  /** Constructs `Value[Str]` from Scala string. Also available as a [[Conversion]]. */
  def str(s: String): Value[Str] = fromMotif(ValueMotif.StringValue(s))

  /** Constructs `Value[Int32]` from Scala Int. Also available as a [[Conversion]]. */
  def int32(i: Int): Value[Int32] = fromMotif(ValueMotif.Int32Value(i))

  /** Constructs `Value[Int64]` from Scala Long. Also available as a [[Conversion]]. */
  def int64(i: Long): Value[Int64] = fromMotif(ValueMotif.Int64Value(i))

  /** Constructs `Value[Bool]` from Scala Boolean. Also available as a [[Conversion]]. */
  def bool(b: Boolean): Value[Bool] = fromMotif(ValueMotif.BoolValue(b))

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

  /** Constructs `Value[Enum[Base, Cases]]` from a corresponding Scala primitive. Also available as a [[Conversion]]. */
  def enm[Base, Cases](value: ScalaUnionOf[Cases])(using ev: ScalaValueOf[value.type, Base]): Value[Enum[Base, Cases]] =
    fromMotif(ValueMotif.EnumValue[Base, Cases, value.type](ev))

  given enumConversion[Base, Cases, T](using
    ev1: T <:< ScalaUnionOf[Cases],
    ev3: (T QualifiesAs Base),
  ): Conversion[T, Value[Enum[Base, Cases]] | None.type] =
    (t: T) =>
      Subtype(summon[t.type <:< ScalaUnionOf[Cases]]) match
        case Subtype.Refl() =>
          val u = ScalaValueOf[T, Base](t)
          mkEnum[Base, Cases, t.type](u)

  @targetName("arrStr")
  def arr(elems: String*): Value[Arr[Str]] =
    arr(elems.map(str)*)

  @targetName("arrInt32")
  def arr(elems: Int*): Value[Arr[Int32]] =
    arr(elems.map(int32)*)

  @targetName("arrInt64")
  def arr(elems: Long*): Value[Arr[Int64]] =
    arr(elems.map(int64)*)

  object Obj {
    val empty: Value[Obj[Void]] =
      fromMotif(ValueMotif.Object.empty)

    def fromTuple[Props](
      t: PropTypesTupleU[Value, Props],
    )(using
      ps: PropertyList[Props],
    ): Value[Obj[Props]] =
      fromMotif:
        ValueMotif.Object[Value, Props]:
          ps.readNamedTuple[Value, [x] =>> Value[x] | None.type, Value, Optional[Value]](t)(
            [A] => (va: Value[A]) => va,
            [A] => (va: Value[A] | None.type) => toOption(va),
          )

    def apply[Props](
      f: ObjectBuilderFromNamedTuple[Props, PropNamesTuple[Props], PropTypesTupleU[Value, Props]] => Value[Obj[Props]],
    )(using
      PropertyList[Props],
    ): Value[Obj[Props]] =
      f(ObjectBuilderFromNamedTuple[Props, PropNamesTuple[Props], PropTypesTupleU[Value, Props]])

    def builder[Props](
      f: ObjectBuilder[Void, ToRightAssoc[Props]] => ObjectBuilder[Props, Void],
    ): Value[Obj[Props]] =
      f(ObjectBuilder[Props]).result

    // The named tuple in the return type does not reduce in IDE hints.
    // Revisit after https://github.com/scalameta/metals/issues/7556 is resolved.
    def unapply[Props](v: Value[Obj[Props]]): Some[NamedTuple[PropNamesTuple[Props], PropTypesTupleO[Value, Props]]] =
      Some(v.toNamedTuple())

    class ObjectBuilderFromNamedTuple[Props, N <: PropNamesTuple[Props], T <: PropTypesTupleU[Value, Props]](using
      PropertyList[Props],
    ) {
      def apply(t: NamedTuple[N, T]): Value[Obj[Props]] =
        fromTuple[Props](t)
    }
  }

  extension [Ps](value: Value[Obj[Ps]]) {
    def set[T](k: String, v: Value[T]): Value[Obj[Ps || k.type :: T]] =
      extend(SingletonType(k), v)

    def setOpt[T](k: String, v: Value[T]): Value[Obj[Ps || k.type :? T]] =
      extendOpt(SingletonType(k), Some(v))

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

    def skip(propName: Label)(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :? A, Tail] =
      ValueMotif.Object.extendOpt(b, l, None)
  }

  extension [Acc, Label <: String, Cases, Tail](b: ObjectBuilder[Acc, Label :? Enum[Str, Cases] || Tail])
    @targetName("setEnumOpt")
    def set(propName: Label, value: ScalaUnionOf[Cases] & String)(using l: SingletonType[Label]): ObjectBuilder[Acc || Label :? Enum[Str, Cases], Tail] =
      ValueMotif.Object.extendOpt(b, l, Some(mkEnum(ScalaValueOf.str(value))))

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
    def get[K <: NamesOf[Ps]](using i: IsPropertyOf[K, Ps]): i.ReqOrOpt[Value, Optional[Value]][i.Type] =
      // keeps failing on incremental compilation due to https://github.com/sbt/zinc/issues/1561
      toMotifObject(value).get[K]

    /** Intermediary for accessing `Obj`ect's properties.
     *
     * Methods on the resulting object get better IDE hints than extension methods directly on `Value[Obj[...]]`,
     * likely due to https://github.com/scalameta/metals/issues/7556.
     */
    def props: PropGetter[Ps, NamesOf[Ps]] =
      PropGetter(toMotifObject(value))

    def toNamedTuple: ObjToNamedTuple[Ps, PropNamesTuple[Ps], PropTypesTupleO[Value, Ps], PropTypesTupleU[Value, Ps]] =
      ObjToNamedTuple(toMotifObject(value))
  }

  class ObjToNamedTuple[Ps, Ns <: Tuple, Ts <: Tuple, Us <: Tuple](
    value: ValueMotif.Object[Value, Ps],
  )(using
    evN: Ns =:= PropNamesTuple[Ps],
    evT: Ts =:= PropTypesTupleO[Value, Ps],
    evU: Us =:= PropTypesTupleU[Value, Ps],
  ) {
    def options: NamedTuple[Ns, Ts] =
      TypeEq(evN.flip).substUpperBounded[Tuple, [ns <: Tuple] =>> NamedTuple[ns, Ts]]:
        TypeEq(evT.flip).substUpperBounded[Tuple, [ts <: Tuple] =>> NamedTuple[PropNamesTuple[Ps], ts]]:
          value.toNamedTuple

    def orNones: NamedTuple[Ns, Us] =
      TypeEq(evN.flip).substUpperBounded[Tuple, [ns <: Tuple] =>> NamedTuple[ns, Us]]:
        TypeEq(evU.flip).substUpperBounded[Tuple, [us <: Tuple] =>> NamedTuple[PropNamesTuple[Ps], us]]:
          value.toNamedTuple[Value, [x] =>> Value[x] | None.type](
            [A] => fa => fa,
            [A] => ofa => ofa.getOrElse(None),
          )

    def apply(): NamedTuple[Ns, Ts] =
      options
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

    def selectDynamic(propName: String): (ev: propName.type IsPropertyOf Ps) ?=> ev.ReqOrOpt[Value, Optional[Value]][ev.Type] =
      (ev: propName.type IsPropertyOf Ps) ?=>
        // keeps failing on incremental compilation due to https://github.com/sbt/zinc/issues/1561
        obj.get[propName.type]

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
    def apply[K <: KeySet](using i: IsPropertyOf[K, Ps]): i.ReqOrOpt[Value, Optional[Value]][i.Type] =
      // keeps failing on incremental compilation due to https://github.com/sbt/zinc/issues/1561
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

  def show[A](va: Value[A]): String =
    val b = new StringBuilder
    showAppend(va)(b)
    b.result()
}
