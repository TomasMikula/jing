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
sealed trait Enum[Base, Cases]
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

type ToRightAssoc[Props] =
  ToRightAssocAcc[Props, Void]

type ToRightAssocAcc[Props, Acc] = Props match
  case init || last => ToRightAssocAcc[init, last || Acc]
  case Void => Acc

type ScalaUnionOf[Cases] = Cases match
  case init || last => ScalaUnionOf[init] | last
  case Void => Nothing