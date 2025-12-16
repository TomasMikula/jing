package jing.openapi

import jing.openapi.model.*
import libretto.lambda.util.SingletonType

private[openapi] infix sealed trait IsRefinedBy[A, B]

private[openapi] object IsRefinedBy {
  case class UnknownRefinedByAnything[T]() extends (Unknown IsRefinedBy T)
  // case class Refl[T]() extends (T IsRefinedBy T)
  case class Trans[A, B, C](aRb: A IsRefinedBy B, bRc: B IsRefinedBy C) extends (A IsRefinedBy C)

  case class Lift_||[A, B, X, Y](aRb: A IsRefinedBy B, xRy: X IsRefinedBy Y) extends (A || X IsRefinedBy B || Y)
  case class LiftEnum[A, B, Cs, Ds](aRb: A IsRefinedBy B, cRd: Cs IsRefinedBy Ds) extends (Enum[A, Cs] IsRefinedBy Enum[B, Ds])
  case class LiftConst[A, B](aRb: A IsRefinedBy B) extends (Const[A] IsRefinedBy Const[B])
  case class LiftArr[A, B](aRb: A IsRefinedBy B) extends (Arr[A] IsRefinedBy Arr[B])
  case class LiftObj[A, B](aRb: A IsRefinedBy B) extends (Obj[A] IsRefinedBy Obj[B])
  case class LiftDiscUnion[A, B](aRb: A IsRefinedBy B) extends (DiscriminatedUnion[A] IsRefinedBy DiscriminatedUnion[B])
  case class Lift_::[Lbl <: String, A, B](lbl: SingletonType[Lbl], aRb: A IsRefinedBy B) extends (Lbl :: A IsRefinedBy Lbl :: B)
  case class Lift_:?[Lbl <: String, A, B](lbl: SingletonType[Lbl], aRb: A IsRefinedBy B) extends (Lbl :? A IsRefinedBy Lbl :? B)
  case class LiftOops[A, B](aRb: A IsRefinedBy B) extends (Oops[A] IsRefinedBy Oops[B])
  case object LiftVoid extends (Void IsRefinedBy Void)
  case object LiftInt32 extends (Int32 IsRefinedBy Int32)
  case object LiftInt64 extends (Int64 IsRefinedBy Int64)
  case object LiftStr extends (Str IsRefinedBy Str)
  case object LiftBool extends (Bool IsRefinedBy Bool)
  case class LiftSingletonInt[T <: Int](t: SingletonType[T]) extends (T IsRefinedBy T)
  case class LiftSingletonLong[T <: Long](t: SingletonType[T]) extends (T IsRefinedBy T)
  case class LiftSingletonString[T <: String](t: SingletonType[T]) extends (T IsRefinedBy T)
  case class LiftSingletonBoolean[T <: Boolean](t: SingletonType[T]) extends (T IsRefinedBy T)

  given Compatible[IsRefinedBy] {

    override def lift_||[A, B, X, Y](aRb: A IsRefinedBy B, xRy: X IsRefinedBy Y): A || X IsRefinedBy B || Y = Lift_||(aRb, xRy)

    override def lift_enum[A, B, Cs, Ds](aRb: A IsRefinedBy B, cRd: Cs IsRefinedBy Ds): Enum[A, Cs] IsRefinedBy Enum[B, Ds] = LiftEnum(aRb, cRd)

    extension [A, B](aRb: A IsRefinedBy B) {
      override def lift_const: Const[A] IsRefinedBy Const[B] = LiftConst(aRb)

      override def lift_arr: Arr[A] IsRefinedBy Arr[B] = LiftArr(aRb)

      override def lift_obj: Obj[A] IsRefinedBy Obj[B] = LiftObj(aRb)

      override def lift_discriminatedUnion: DiscriminatedUnion[A] IsRefinedBy DiscriminatedUnion[B] = LiftDiscUnion(aRb)

      override def lift_-::-[Lbl <: String](using lbl: SingletonType[Lbl]): Lbl :: A IsRefinedBy Lbl :: B = Lift_::(lbl, aRb)

      override def lift_-:?-[Lbl <: String](using lbl: SingletonType[Lbl]): Lbl :? A IsRefinedBy Lbl :? B = Lift_:?(lbl, aRb)

      override def lift_oops: Oops[A] IsRefinedBy Oops[B] = LiftOops(aRb)
    }

    override def lift_void: Void IsRefinedBy Void = LiftVoid

    override def lift_int32: Int32 IsRefinedBy Int32 = LiftInt32

    override def lift_int64: Int64 IsRefinedBy Int64 = LiftInt64

    override def lift_str: Str IsRefinedBy Str = LiftStr

    override def lift_bool: Bool IsRefinedBy Bool = LiftBool

    override def lift_singletonInt[T <: Int](t: SingletonType[T]): T IsRefinedBy T = LiftSingletonInt(t)

    override def lift_singletonLong[T <: Long](t: SingletonType[T]): T IsRefinedBy T = LiftSingletonLong(t)

    override def lift_singletonString[T <: String](t: SingletonType[T]): T IsRefinedBy T = LiftSingletonString(t)

    override def lift_singletonBoolean[T <: Boolean](t: SingletonType[T]): T IsRefinedBy T = LiftSingletonBoolean(t)

  }
}
