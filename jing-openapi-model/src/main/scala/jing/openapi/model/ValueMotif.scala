package jing.openapi.model

import libretto.lambda.Items1Named
import libretto.lambda.Items1Named.Member
import libretto.lambda.util.{BiInjective, TypeEqK, TypeEq}
import libretto.lambda.util.TypeEq.Refl
import scala.annotation.targetName
import libretto.lambda.util.Applicative
import libretto.lambda.util.SingletonType

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
        def go[Ps](o: ObjectMotif[Object.Payload[F], Ps]): Unit = {
          o match
            case ObjectMotif.Empty() => // do nothing
            case ObjectMotif.Snoc(init, lastName, lastValue) =>
              go(init)
              b.append(lastName.value: String)
              b.append(": ")
              f(lastValue, b)
              b.append(",")
            case so: ObjectMotif.SnocOpt[pf, init, k, v] =>
              go(so.init)
              (so.pval: Option[F[v]]) match
                case None =>
                  // do nothing
                case Some(v) =>
                  b.append(so.pname.value: String)
                  b.append(": ")
                  f(v, b)
                  b.append(",")
        }
        go((obj: Object[f, ps]).asInstanceOf[Object[F, ps]].value) // TODO: remove unsafe cast when https://github.com/scala/scala3/issues/22993 is fixed
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

  case class Object[F[_], Ps](
    value: ObjectMotif[Object.Payload[F], Ps],
  ) extends ValueMotif[F, Obj[Ps]] {

    def getPayload[K](using i: IsPropertyOf[K, Ps]): Object.Payload[F][i.Mod, i.Type] =
      value.get[K]

    def get[K](using i: IsPropertyOf[K, Ps]): i.Modality[F[i.Type]] =
      Object.Payload.toModality(i):
        getPayload[K]

    def traverseObj[M[_], G[_]](f: [A] => F[A] => M[G[A]])(using M: Applicative[M]): M[ValueMotif.Object[G, Ps]] =
      value.traverse[M, Object.Payload[G]](
        f,
        [a] => (ofa: Option[F[a]]) => ofa.fold(M.pure(None))(fa => f(fa).map(Some(_))),
      ).map(Object(_))

  }

  object Object {
    type Payload[+F[_]] =
      [M <: ObjectMotif.Mod, A] =>>
        M match
          case ObjectMotif.Mod.Required.type => F[A]
          case ObjectMotif.Mod.Optional.type => Option[F[A]]

    object Payload {
      def toModality[K, Ps, F[_]](i: K IsPropertyOf Ps)(value: Payload[F][i.Mod, i.Type]): i.Modality[F[i.Type]] =
        i.modAndModalityInterlocked match {
          case Left((ev1, ev2)) =>
            val v: F[i.Type] =
              TypeEq(ev1).substUpperBounded[ObjectMotif.Mod, [m <: ObjectMotif.Mod] =>> Payload[F][m, i.Type]]:
                value
            ev2.flip.at[F[i.Type]]:
              v
          case Right((ev1, ev2)) =>
            val v: Option[F[i.Type]] =
              TypeEq(ev1).substUpperBounded[ObjectMotif.Mod, [m <: ObjectMotif.Mod] =>> Payload[F][m, i.Type]]:
                value
            ev2.flip.at[F[i.Type]]:
              v
        }
    }

    def empty[F[_]]: ValueMotif.Object[F, Void] =
      Object(ObjectMotif.Empty())

    def extend[F[_], Base, K <: String, V](
      base: ValueMotif[F, Obj[Base]],
      k: SingletonType[K],
      v: F[V],
    ): ValueMotif.Object[F, Base || K :: V] =
      Object(ObjectMotif.Snoc(asObject(base).value, k, v))

    def extendOpt[F[_], Base, K <: String, V](
      base: ValueMotif[F, Obj[Base]],
      k: SingletonType[K],
      v: Option[F[V]],
    ): ValueMotif.Object[F, Base || K :? V] =
      Object(ObjectMotif.SnocOpt(asObject(base).value, k, v))

    extension [F[_], Ps](value: ValueMotif[F, Obj[Ps]]) {
      private def asObject: ValueMotif.Object[F, Ps] =
        value match
          case o: ValueMotif.Object[F, ps] => o
    }

    extension [F[_], Init, K <: String, V](value: Object[F, Init || K :: V])
      def unsnoc: (Object[F, Init], F[V]) =
        value.value match
          case ObjectMotif.Snoc(init, lastName, lastValue) => (Object(init), lastValue)

    extension [F[_], Init, K <: String, V](value: Object[F, Init || K :? V])
      @targetName("unsnocOpt")
      def unsnoc: (Object[F, Init], Option[F[V]]) =
        value.value match
          case ObjectMotif.SnocOpt(init, lastName, lastValue) => (Object(init), lastValue)
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
    def scalaValue: ScalaUnionOf[Cases] =
      value match
        case EnumValue(value) =>
          value.get

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
