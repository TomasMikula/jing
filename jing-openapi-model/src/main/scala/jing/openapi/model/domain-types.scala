package jing.openapi.model

import libretto.lambda.util.{BiInjective, TypeEq}
import libretto.lambda.util.TypeEq.Refl

import scala.NamedTuple.AnyNamedTuple

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
sealed trait Oops[Reason]

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
      kv match
        case k :: _ => PropNamesTupleAcc[init, k *: Acc]
        case k :? _ => PropNamesTupleAcc[init, k *: Acc]

type PropTypesTupleF[F[_ <: ObjectMotif.Mod, _], Props] =
  PropTypesTupleFAcc[F, Props, EmptyTuple]

type PropTypesTupleFAcc[F[_ <: ObjectMotif.Mod, _], Props, Acc <: Tuple] <: Tuple =
  Props match
    case Void =>
      Acc
    case init || kv =>
      kv match
        case _ :: v => PropTypesTupleFAcc[F, init, F[ObjectMotif.Mod.Required.type, v] *: Acc]
        case _ :? v => PropTypesTupleFAcc[F, init, F[ObjectMotif.Mod.Optional.type, v] *: Acc]

type Options[F[_]] = [M <: ObjectMotif.Mod, A] =>>
  M match
    case ObjectMotif.Mod.Required.type => F[A]
    case ObjectMotif.Mod.Optional.type => Option[F[A]]

type OrNones[F[_]] = [M <: ObjectMotif.Mod, A] =>>
  M match
    case ObjectMotif.Mod.Required.type => F[A]
    case ObjectMotif.Mod.Optional.type => F[A] | None.type

type PropTypesTupleO[F[_], Props] =
  PropTypesTupleF[Options[F], Props]

type PropTypesTupleOAcc[F[_], Props, Acc <: Tuple] =
  PropTypesTupleFAcc[Options[F], Props, Acc]

type PropTypesTupleU[F[_], Props] =
  PropTypesTupleF[OrNones[F], Props]

type PropTypesTupleUAcc[F[_], Props, Acc <: Tuple] =
  PropTypesTupleFAcc[OrNones[F], Props, Acc]

type PropsToNamedTuple[F[_], Props] =
  PropsToNamedTupleAcc[F, Props, NamedTuple.Empty]

type PropsToNamedTupleAcc[F[_], Props, Acc <: AnyNamedTuple] <: AnyNamedTuple =
  Props match
    case Void =>
      Acc
    case init || kv =>
      kv match
        case k :: v => PropsToNamedTupleAcc[F, init, NamedTuples.Cons[k, F[v], Acc]]
        case k :? v => PropsToNamedTupleAcc[F, init, NamedTuples.Cons[k, F[v] | None.type, Acc]]