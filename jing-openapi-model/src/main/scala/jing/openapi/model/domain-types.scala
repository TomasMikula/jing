package jing.openapi.model

import libretto.lambda.Items1Named.Member
import libretto.lambda.util.TypeEq.Refl
import libretto.lambda.util.{BiInjective, TypeEq}

sealed trait Int32
sealed trait Int64
sealed trait Str
sealed trait Bool
sealed trait Arr[A]
sealed trait Obj[Props]
sealed trait ||[A, B]
sealed trait ::[A, B]
sealed trait :?[A, B]
sealed trait DiscriminatedUnion[Variants]
sealed trait Enum[Base, Cases]
sealed trait Const[A]
sealed trait Oops[Reason]

sealed trait Unknown

type ScalaReprOf[T] =
  T match
    case Int32 => Int
    case Int64 => Long
    case Str => String
    case Bool => Boolean

// workaround of https://github.com/scala/scala3/issues/22943
type LabelOf[KV] = KV match
  case k :: v => k

type DiscriminatorOf[Cases] =
  Cases match
    case s :: t => s
    case init || last => DiscriminatorOf[init] | LabelOf[last]

object DiscriminatorOf {
  def from[Lbl, Cases](i: Lbl IsCaseOf Cases): Lbl & String & DiscriminatorOf[Cases] =
    fromMember(IsCaseOf.toMember(i))

  def fromMember[Lbl, A, Cases](member: Member[||, ::, Lbl, A, Cases]): Lbl & String & DiscriminatorOf[Cases] = {
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
        summon[(init || lblB :: b) =:= Cases].substituteCo[[cs] =>> Lbl & String & DiscriminatorOf[cs]](
          (fromMember[Lbl, A, init](i.i)
            : Lbl & String & DiscriminatorOf[init])
            : Lbl & String & DiscriminatorOf[init || lblB :: b]
        )
  }
}

object || {
  given BiInjective[||] with {
    override def unapply[A, B, X, Y](ev: (A || B) =:= (X || Y)): (A =:= X, B =:= Y) =
      ev match { case TypeEq(Refl()) => (summon, summon) }
  }

  def isNotVoid[A, B](using (A || B) =:= Void): Nothing =
    throw AssertionError("Impossible: (A || B) =:= Void")
}

object :: {
  given BiInjective[::] with {
    override def unapply[A, B, X, Y](ev: A :: B =:= X :: Y): (A =:= X, B =:= Y) =
      ev match { case TypeEq(Refl()) => (summon, summon) }
  }
}

object  :? {
  given BiInjective[:?] with {
    override def unapply[A, B, X, Y](ev: A :? B =:= X :? Y): (A =:= X, B =:= Y) =
      ev match { case TypeEq(Refl()) => (summon, summon) }
  }

  def isNot_::[A, B, C, D](using (A :? B) =:= (C :: D)): Nothing =
    throw AssertionError("Impossible: (A :? B) =:= (C :: D)")
}

type ToRightAssoc[Props] =
  ToRightAssocAcc[Props, Void]

type ToRightAssocAcc[Props, Acc] = Props match
  case init || last => ToRightAssocAcc[init, last || Acc]
  case Void => Acc

type NamedCasesToRightAssoc[Cases] = Cases match
  case key :: value => key :: value
  case init || last => NamedCasesToRightAssocAcc[init, last]

type NamedCasesToRightAssocAcc[Cases, Acc] = Cases match
  case init || last => NamedCasesToRightAssocAcc[init, last || Acc]
  case key :: value => (key :: value) || Acc

type ScalaUnionOf[Cases] = Cases match
  case init || last => ScalaUnionOf[init] | last
  case Void => Nothing

type NamesOf[NamedCases] = NamedCases match
  case init || kv =>
    kv match
      case k :: _ => NamesOf[init] | k
      case k :? _ => NamesOf[init] | k
  case Void =>
    Nothing

type PropNamesTuple[Props] =
  PropNamesTupleAcc[Props, EmptyTuple]

type PropNamesTupleAcc[Props, Acc <: Tuple] <: Tuple =
  Props match
    case Void =>
      Acc
    case init || kv =>
      PropNamesTupleAcc[init, PropName[kv] *: Acc]

type PropName[P] =
  P match
    case k :: _ => k
    case k :? _ => k

type PropTypesTupleF[Req[_], Opt[_], Props] =
  PropTypesTupleFAcc[Req, Opt, Props, EmptyTuple]

type PropTypesTupleFAcc[Req[_], Opt[_], Props, Acc <: Tuple] <: Tuple =
  Props match
    case Void =>
      Acc
    case init || kv =>
      PropTypesTupleFAcc[Req, Opt, init, PropTypeF[Req, Opt, kv] *: Acc]

type PropTypeF[Req[_], Opt[_], Prop] =
  Prop match
    case _ :: v => Req[v]
    case _ :? v => Opt[v]

type Optional[F[_]] =
  [A] =>> Option[F[A]]

type PropTypesTupleO[F[_], Props] =
  PropTypesTupleF[F, [x] =>> Option[F[x]], Props]

type PropTypesTupleU[F[_], Props] =
  PropTypesTupleF[F, [x] =>> F[x] | None.type, Props]
