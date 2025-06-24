package jing.openapi.model

import libretto.lambda.Items1Named
import libretto.lambda.Items1Named.Member
import libretto.lambda.util.{BiInjective, TypeEqK, TypeEq}
import libretto.lambda.util.TypeEq.Refl
import scala.annotation.targetName
import libretto.lambda.util.Applicative

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
        b.append(value.show)
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

  def traverse[M[_], G[_]](f: [A] => F[A] => M[G[A]])(using M: Applicative[M]): M[ValueMotif[G, T]] =
    this match
      case Uno => M.pure(Uno)
      case s: StringValue => M.pure(s)
      case i: Int32Value => M.pure(i)
      case i: Int64Value => M.pure(i)
      case b: BoolValue => M.pure(b)
      case e @ EnumValue(_) => M.pure(e)
      case Array(elems) =>
        Applicative
          .traverseList(elems.toList) { a => f(a) }
          .map(bs => Array(IArray.from(bs)))
      case o: Object[f, ps] =>
        o.asInstanceOf[Object[F, ps]] // https://github.com/scala/scala3/issues/22993
          .traverseObj(f)
          .widen
      case DiscUnion(underlying) =>
        underlying match
          case Items1Named.Sum.Value(tag, value) =>
            f(value)
              .map { v => DiscUnion(Items1Named.Sum.Value(tag, v)) }



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
    import Object.*

    def get[K](using i: IsPropertyOf[K, Ps]): i.Modality[F[i.Type]]

    def traverseObj[M[_], G[_]](f: [A] => F[A] => M[G[A]])(using M: Applicative[M]): M[ValueMotif.Object[G, Ps]]

    def foreachProperty(
      f: [K <: String, V] => (k: K, v: F[V]) => Unit,
    ): Unit
  }

  object Object {
    case object ObjEmpty extends ValueMotif.Object[Nothing, Void] {
      override def get[K](using i: IsPropertyOf[K, Void]): i.Modality[Nothing] =
        i.propertiesNotVoid

      override def traverseObj[M[_], G[_]](f: [A] => Nothing => M[G[A]])(using M: Applicative[M]): M[Object[G, Void]] =
        M.pure(ObjEmpty)

      override def foreachProperty(f: [K <: String, V] => (k: K, v: Nothing) => Unit): Unit =
        () // do nothing
    }

    case class ObjExt[F[_], Init, PropName <: String, PropType](
      init: Object[F, Init],
      lastName: PropName,
      lastValue: F[PropType],
    ) extends ValueMotif.Object[F, Init || PropName :: PropType] {
      override def get[K](using i: IsPropertyOf[K, Init || PropName :: PropType]): i.Modality[F[i.Type]] =
        i.switch[i.Modality[F[i.Type]]](
          caseLastProp =
            [init] => (
              ev1: (Init || PropName :: PropType) =:= (init || K :: i.Type),
              ev2: TypeEqK[i.Modality, [A] =>> A],
            ) => {
              ev1 match
                case BiInjective[||](_, BiInjective[::](_, TypeEq(Refl()))) =>
                  ev2.flip.at[F[i.Type]](lastValue)
            },
          caseOptLastProp =
            [init] => (
              ev1: (Init || PropName :: PropType) =:= (init || K :? i.Type),
              ev2: TypeEqK[i.Modality, Option],
            ) => {
              ev1 match
                case BiInjective[||](_, ev) => :?.isNot_::[K, i.Type, PropName, PropType](using ev.flip)
            },
          caseInitProp =
            [init, last] => (
              ev1: (Init || PropName :: PropType) =:= (init || last),
              j:   IsPropertyOf.Aux[K, init, i.Type, i.Modality],
            ) => {
              ev1 match
                case BiInjective[||](TypeEq(Refl()), _) =>
                  init.get[K](using j)
            },
        )

      override def traverseObj[M[_], G[_]](f: [A] => F[A] => M[G[A]])(using M: Applicative[M]): M[Object[G, Init || PropName :: PropType]] =
        M.map2(init.traverseObj(f), f(lastValue)):
          (gInit, gLast) => ObjExt(gInit, lastName, gLast)

      override def foreachProperty(f: [K <: String, V] => (k: K, v: F[V]) => Unit): Unit =
        init.foreachProperty(f)
        f(lastName, lastValue)
    }

    case class ObjExtOpt[F[_], Init, PropName <: String, PropType](
      init: Object[F, Init],
      lastName: PropName,
      lastValue: Option[F[PropType]],
    ) extends ValueMotif.Object[F, Init || PropName :? PropType] {
      override def get[K](using i: IsPropertyOf[K, Init || PropName :? PropType]): i.Modality[F[i.Type]] =
        i.switch[i.Modality[F[i.Type]]](
          caseLastProp =
            [init] => (
              ev1: (Init || PropName :? PropType) =:= (init || K :: i.Type),
              ev2: TypeEqK[i.Modality, [A] =>> A],
            ) => {
              ev1 match
                case BiInjective[||](_, ev) =>
                  :?.isNot_::(using ev)
            },
          caseOptLastProp =
            [init] => (
              ev1: (Init || PropName :? PropType) =:= (init || K :? i.Type),
              ev2: TypeEqK[i.Modality, Option],
            ) => {
              ev1 match
                case BiInjective[||](_, BiInjective[:?](_, TypeEq(Refl()))) =>
                  ev2.flip.at[F[i.Type]](lastValue)
            },
          caseInitProp =
            [init, last] => (
              ev1: (Init || PropName :? PropType) =:= (init || last),
              j:   IsPropertyOf.Aux[K, init, i.Type, i.Modality],
            ) => {
              ev1 match
                case BiInjective[||](TypeEq(Refl()), _) =>
                  init.get[K](using j)
            },
        )

      override def traverseObj[M[_], G[_]](f: [A] => F[A] => M[G[A]])(using M: Applicative[M]): M[Object[G, Init || PropName :? PropType]] =
        val mgLast: M[Option[G[PropType]]] =
          lastValue.fold(M.pure(None))(f(_).map(Some(_)))
        M.map2(init.traverseObj(f), mgLast):
          (gInit, gLast) => ObjExtOpt(gInit, lastName, gLast)

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
