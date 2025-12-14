package jing.openapi.model

import libretto.lambda.util.SingletonType

/** Witnesses that the binary relation `Rel` on types is _compatible_ with all
 *  type constructors of the OpenAPI domain (as modeled by JING).
 */
trait Compatible[Rel[_, _]] {

  /* Compatibility with binary operations */

  def lift_||[A, B, X, Y](aRb: A `Rel` B, xRy: X `Rel` Y): (A || X) `Rel` (B || Y)
  def lift_enm[A, B, Cs, Ds](aRb: A `Rel` B, cRd: Cs `Rel` Ds): Enum[A, Cs] `Rel` Enum[B, Ds]


  /* Compatibility with unary operations */

  extension [A, B](aRb: A `Rel` B) {
    def lift_const: Const[A] `Rel` Const[B]
    def lift_arr: Arr[A] `Rel` Arr[B]
    def lift_obj: Obj[A] `Rel` Obj[B]
    def lift_discriminatedUnion: DiscriminatedUnion[A] `Rel` DiscriminatedUnion[B]
    def lift_-::-[Lbl <: String](using SingletonType[Lbl]): (Lbl :: A) `Rel` (Lbl :: B)
    def lift_-:?-[Lbl <: String](using SingletonType[Lbl]): (Lbl :? A) `Rel` (Lbl :? B)
    def lift_oops: Oops[A] `Rel` Oops[B]
  }


  /* Compatibility with nullary operations */

  def lift_void: Void `Rel` Void
  def lift_int32: Int32 `Rel` Int32
  def lift_int64: Int64 `Rel` Int64
  def lift_str: Str `Rel` Str
  def lift_bool: Bool `Rel` Bool
  def lift_singletonInt[T <: Int](t: SingletonType[T]): T `Rel` T
  def lift_singletonLong[T <: Long](t: SingletonType[T]): T `Rel` T
  def lift_singletonString[T <: String](t: SingletonType[T]): T `Rel` T
  def lift_singletonBoolean[T <: Boolean](t: SingletonType[T]): T `Rel` T
}

object Compatible {
  given Compatible[=:=] {
    import jing.openapi.model.*
    import libretto.lambda.util.SingletonType

    override def lift_||[A, B, X, Y](aRb: A =:= B, xRy: X =:= Y): (A || X) =:= (B || Y) =
      aRb.liftCo[[a] =>> a || X] andThen xRy.liftCo[[x] =>> B || x]

    override def lift_enm[A, B, Cs, Ds](aRb: A =:= B, cRd: Cs =:= Ds): Enum[A, Cs] =:= Enum[B, Ds] =
      aRb.liftCo[[a] =>> Enum[a, Cs]] andThen cRd.liftCo[[cs] =>> Enum[B, cs]]

    extension [A, B](aRb: A =:= B) {
      override def lift_const: Const[A] =:= Const[B] = aRb.liftCo[Const]
      override def lift_arr: Arr[A] =:= Arr[B] = aRb.liftCo[Arr]
      override def lift_obj: Obj[A] =:= Obj[B] = aRb.liftCo[Obj]
      override def lift_discriminatedUnion: DiscriminatedUnion[A] =:= DiscriminatedUnion[B] = aRb.liftCo[DiscriminatedUnion]
      override def lift_-::-[Lbl <: String](using SingletonType[Lbl]): (Lbl :: A) =:= (Lbl :: B) = aRb.liftCo[::[Lbl, _]]
      override def lift_-:?-[Lbl <: String](using SingletonType[Lbl]): (Lbl :? A) =:= (Lbl :? B) = aRb.liftCo[:?[Lbl, _]]
      override def lift_oops: Oops[A] =:= Oops[B] = aRb.liftCo[Oops]
    }

    override def lift_void: Void =:= Void = summon
    override def lift_int32: Int32 =:= Int32 = summon
    override def lift_int64: Int64 =:= Int64 = summon
    override def lift_str: Str =:= Str = summon
    override def lift_bool: Bool =:= Bool = summon
    override def lift_singletonInt[T <: Int](t: SingletonType[T]): T =:= T = summon
    override def lift_singletonLong[T <: Long](t: SingletonType[T]): T =:= T = summon
    override def lift_singletonString[T <: String](t: SingletonType[T]): T =:= T = summon
    override def lift_singletonBoolean[T <: Boolean](t: SingletonType[T]): T =:= T = summon
  }
}
