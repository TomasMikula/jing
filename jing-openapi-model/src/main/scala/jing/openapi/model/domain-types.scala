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

type PropsToNamedTuple[F[_], Props] =
  PropsToNamedTupleAcc[F, Props, NamedTuple.Empty]

type PropsToNamedTupleAcc[F[_], Props, Acc <: AnyNamedTuple] <: AnyNamedTuple =
  Props match
    case Void =>
      Acc
    case init || kv =>
      kv match
        case k :: v => PropsToNamedTupleAcc[F, init, NamedTuples.Cons[k, F[v], Acc]]
        case k :? v => PropsToNamedTupleAcc[F, init, NamedTuples.Cons[k, Option[F[v]], Acc]]