package jing.openapi

private[openapi] opaque type Unrelated[A, B] = Unit

private[openapi] object Unrelated {
  def apply[A, B](): Unrelated[A, B] =
    ()

  given Substitutive[Unrelated] with {
    override def refl[A]: Unrelated[A, A] = ()
    override def trans[A, B, C](r: Unrelated[A, B], s: Unrelated[B, C]): Unrelated[A, C] = ()
    override def subst[F[_], A, B](rel: Unrelated[A, B]): Unrelated[F[A], F[B]] = ()
  }

  given Compatible[Unrelated] {
      import jing.openapi.model.*
      import libretto.lambda.util.SingletonType

      override def lift_||[A, B, X, Y](aRb: A `Unrelated` B, xRy: X `Unrelated` Y): (A || X) `Unrelated` (B || Y) = ()
      override def lift_enm[A, B, Cs, Ds](aRb: A `Unrelated` B, cRd: Cs `Unrelated` Ds): Enum[A, Cs] `Unrelated` Enum[B, Ds] = ()

      extension [A, B](aRb: A `Unrelated` B) {
        override def lift_const: Const[A] `Unrelated` Const[B] = ()
        override def lift_arr: Arr[A] `Unrelated` Arr[B] = ()
        override def lift_obj: Obj[A] `Unrelated` Obj[B] = ()
        override def lift_discriminatedUnion: DiscriminatedUnion[A] `Unrelated` DiscriminatedUnion[B] = ()
        override def lift_-::-[Lbl <: String](using SingletonType[Lbl]): (Lbl :: A) `Unrelated` (Lbl :: B) = ()
        override def lift_-:?-[Lbl <: String](using SingletonType[Lbl]): (Lbl :? A) `Unrelated` (Lbl :? B) = ()
        override def lift_oops: Oops[A] `Unrelated` Oops[B] = ()
      }

      override def lift_void: Void `Unrelated` Void = ()
      override def lift_int32: Int32 `Unrelated` Int32 = ()
      override def lift_int64: Int64 `Unrelated` Int64 = ()
      override def lift_str: Str `Unrelated` Str = ()
      override def lift_bool: Bool `Unrelated` Bool = ()
      override def lift_singletonInt[T <: Int](t: SingletonType[T]): T `Unrelated` T = ()
      override def lift_singletonLong[T <: Long](t: SingletonType[T]): T `Unrelated` T = ()
      override def lift_singletonString[T <: String](t: SingletonType[T]): T `Unrelated` T = ()
      override def lift_singletonBoolean[T <: Boolean](t: SingletonType[T]): T `Unrelated` T = ()
  }
}
