package jing.openapi.model

import jing.openapi.model.*
import libretto.lambda.util.{Exists, SingletonType}
import libretto.lambda.util.Exists.Indeed

private[openapi] infix sealed trait IsRefinedBy[A, B]

private[openapi] object IsRefinedBy {
  case class UnknownRefinedByAnything[T]() extends (Unknown IsRefinedBy T)

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

  extension [A, X, T](r: (A || X) IsRefinedBy T)
    def preserves_|| : Exists[[B] =>> Exists[[Y] =>> (T =:= (B || Y), A IsRefinedBy B, X IsRefinedBy Y)]] =
      r match
        case r: Lift_||[a, b, x, y] =>
          Indeed(Indeed((summon[T =:= (b || y)], r.aRb, r.xRy)))

  extension [K, V, T](r: (K :: V) IsRefinedBy T)
    def preserves_-:: : Exists[[W] =>> (T =:= (K :: W), V IsRefinedBy W)] =
      r match
        case r: Lift_::[k, v, w] =>
          Indeed((summon[T =:= (K :: w)], r.aRb))

  extension [K, V, T](r: (K :? V) IsRefinedBy T)
    def preserves_-:? : Exists[[W] =>> (T =:= (K :? W), V IsRefinedBy W)] =
      r match
        case r: Lift_:?[k, v, w] =>
          Indeed((summon[T =:= (K :? w)], r.aRb))

  extension [Ps, T](r: Obj[Ps] IsRefinedBy T)
    def preservesObj: Exists[[Qs] =>> (T =:= Obj[Qs], Ps IsRefinedBy Qs)] =
      r match
        case r: LiftObj[ps, qs] =>
          Indeed((summon[T =:= Obj[qs]], r.aRb))

  extension [Ps, Qs](r: Obj[Ps] IsRefinedBy Obj[Qs])
    def unliftObj: Ps IsRefinedBy Qs =
      r match
        case LiftObj(aRb) =>
          aRb

  extension [C, T](r: Const[C] IsRefinedBy T)
    def preservesConst: Exists[[D] =>> (T =:= Const[D], C IsRefinedBy D)] =
      r match
        case r: LiftConst[c, d] =>
          Indeed((summon[T =:= Const[d]], r.aRb))

  extension [A, Cs, T](r: Enum[A, Cs] IsRefinedBy T)
    def preservesEnum: Exists[[B] =>> Exists[[Ds] =>> (T =:= Enum[B, Ds], A IsRefinedBy B, Cs IsRefinedBy Ds)]] =
      r match
        case r: LiftEnum[a, b, cs, ds] =>
          Indeed(Indeed((summon[T =:= Enum[b, ds]], r.aRb, r.cRd)))

  extension [S <: String, T](r: S IsRefinedBy T)
    def isStringRefl: S =:= T =
      r match
        case LiftSingletonString(_) =>
          summon[S =:= T]
        case LiftSingletonInt(s) =>
          summon[S =:= T]
          sys.error(s"Absurd: ${s.value: Int & String}: Int & String")
        case LiftSingletonLong(s) =>
          summon[S =:= T]
          sys.error(s"Absurd: ${s.value: Long & String}: Long & String")
        case LiftSingletonBoolean(s) =>
          summon[S =:= T]
          sys.error(s"Absurd: ${s.value: Boolean & String}: Boolean & String")

  extension [T](r: Str IsRefinedBy T)
    def isStrRefl: Str =:= T =
      r match
        case LiftStr =>
          summon[Str =:= T]

  extension [T](r: Void IsRefinedBy T)
    def isVoidRefl: Void =:= T =
      r match
        case LiftVoid =>
          summon[Void =:= T]

}
