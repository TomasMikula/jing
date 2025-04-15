package jing.openapi.model

import libretto.lambda.util.{BiInjective, TypeEq}
import libretto.lambda.util.TypeEq.Refl

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
sealed trait Oops[Reason]

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
}

type ToRightAssoc[Props, Acc] = Props match
  case init || last => ToRightAssoc[init, last || Acc]
  case Void => Acc