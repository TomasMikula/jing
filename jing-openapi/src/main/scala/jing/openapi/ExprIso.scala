package jing.openapi

import jing.openapi.model.Compatible
import scala.quoted.{Expr, Quotes, Type}

trait ExprIso[F[_, _]] {
  def refl[A](using Type[A]): F[A, A]
  def trans[A, B, C](f: F[A, B], g: F[B, C]): F[A, C]
  def symm[A, B](f: F[A, B]): F[B, A]

  extension [A, B](f: F[A, B]) {
    def apply(using Quotes): Expr[A] => Expr[B]
    def unapply(using Quotes): Expr[B] => Expr[A]
    def lift[M[_]](using Quotes, Type[M]): F[M[A], M[B]]
    def flip: F[B, A] = symm(f)
    infix def andThen[C](g: F[B, C]): F[A, C] = trans(f, g)
  }

  def biLift[A, B, X, Y](
    r: F[A, X],
    s: F[B, Y],
  )[H[_, _]](using Quotes, Type[H]): F[H[A, B], H[X, Y]]
}

object ExprIso {
  given ExprIso[=:=] with {
    override def refl[A](using Type[A]): A =:= A = summon
    override def trans[A, B, C](f: A =:= B, g: B =:= C): A =:= C = f.andThen(g)
    override def symm[A, B](f: A =:= B): B =:= A = f.flip

    extension [A, B](f: A =:= B) {
      override def apply(using Quotes): Expr[A] => Expr[B] =
        f.substituteCo[Expr](_: Expr[A])

      override def unapply(using Quotes): Expr[B] => Expr[A] =
        f.substituteContra[Expr](_: Expr[B])

      override def lift[M[_]](using Quotes, Type[M]): M[A] =:= M[B] =
        f.liftCo[M]
    }

    override def biLift[A, B, X, Y](r: A =:= X, s: B =:= Y)[H[_, _]](using Quotes, Type[H]): H[A, B] =:= H[X, Y] =
      r.liftCo[[x] =>> H[x, B]] andThen s.liftCo[[y] =>> H[X, y]]
  }

  sealed trait CompileTimeCompatible[A, B] {
    def l: Type[A]
    def r: Type[B]
  }

  object CompileTimeCompatible {
    private case class Unsafe[A, B](l: Type[A], r: Type[B]) extends CompileTimeCompatible[A, B]

    def unsafeAssert[A, B](using a: Type[A], b: Type[B]): CompileTimeCompatible[A, B] =
      Unsafe(a, b)

    given Quotes => Compatible[CompileTimeCompatible] {
      import jing.openapi.model.*
      import libretto.lambda.util.SingletonType
      import ModelToScalaAst.{quotedSingletonBoolean, quotedSingletonInt, quotedSingletonLong, quotedSingletonString}

      override def lift_||[A, B, X, Y](
        aRb: A `CompileTimeCompatible` B,
        xRy: X `CompileTimeCompatible` Y,
      ): (A || X) `CompileTimeCompatible` (B || Y) =
        given Type[A] = aRb.l
        given Type[B] = aRb.r
        given Type[X] = xRy.l
        given Type[Y] = xRy.r
        unsafeAssert[A || X, B || Y]

      override def lift_enm[A, B, Cs, Ds](
        aRb: A `CompileTimeCompatible` B,
        cRd: Cs `CompileTimeCompatible` Ds,
      ): Enum[A, Cs] `CompileTimeCompatible` Enum[B, Ds] =
        given Type[A] = aRb.l
        given Type[B] = aRb.r
        given Type[Cs] = cRd.l
        given Type[Ds] = cRd.r
        unsafeAssert[Enum[A, Cs] , Enum[B, Ds]]

      extension [A, B](aRb: A `CompileTimeCompatible` B) {
        override def lift_const: Const[A] `CompileTimeCompatible` Const[B] =
          given Type[A] = aRb.l
          given Type[B] = aRb.r
          unsafeAssert

        override def lift_arr: Arr[A] `CompileTimeCompatible` Arr[B] =
          given Type[A] = aRb.l
          given Type[B] = aRb.r
          unsafeAssert

        override def lift_obj: Obj[A] `CompileTimeCompatible` Obj[B] =
          given Type[A] = aRb.l
          given Type[B] = aRb.r
          unsafeAssert

        override def lift_discriminatedUnion: DiscriminatedUnion[A] `CompileTimeCompatible` DiscriminatedUnion[B] =
          given Type[A] = aRb.l
          given Type[B] = aRb.r
          unsafeAssert

        override def lift_-::-[Lbl <: String](using lbl: SingletonType[Lbl]): (Lbl :: A) `CompileTimeCompatible` (Lbl :: B) =
          given Type[Lbl] = quotedSingletonString(lbl)._1
          given Type[A] = aRb.l
          given Type[B] = aRb.r
          unsafeAssert

        override def lift_-:?-[Lbl <: String](using lbl: SingletonType[Lbl]): (Lbl :? A) `CompileTimeCompatible` (Lbl :? B) =
          given Type[Lbl] = quotedSingletonString(lbl)._1
          given Type[A] = aRb.l
          given Type[B] = aRb.r
          unsafeAssert

        override def lift_oops: Oops[A] `CompileTimeCompatible` Oops[B] =
          given Type[A] = aRb.l
          given Type[B] = aRb.r
          unsafeAssert
      }

      override def lift_void: Void `CompileTimeCompatible` Void = unsafeAssert
      override def lift_int32: Int32 `CompileTimeCompatible` Int32 = unsafeAssert
      override def lift_int64: Int64 `CompileTimeCompatible` Int64 = unsafeAssert
      override def lift_str: Str `CompileTimeCompatible` Str = unsafeAssert
      override def lift_bool: Bool `CompileTimeCompatible` Bool = unsafeAssert

      override def lift_singletonInt[T <: Int](t: SingletonType[T]): T `CompileTimeCompatible` T =
        given Type[T] = quotedSingletonInt(t)._1
        unsafeAssert

      override def lift_singletonLong[T <: Long](t: SingletonType[T]): T `CompileTimeCompatible` T =
        given Type[T] = quotedSingletonLong(t)._1
        unsafeAssert

      override def lift_singletonString[T <: String](t: SingletonType[T]): T `CompileTimeCompatible` T =
        given Type[T] = quotedSingletonString(t)._1
        unsafeAssert

      override def lift_singletonBoolean[T <: Boolean](t: SingletonType[T]): T `CompileTimeCompatible` T =
        given Type[T] = quotedSingletonBoolean(t)._1
        unsafeAssert
    }

    given ExprIso[CompileTimeCompatible] {
      override def refl[A](using a: Type[A]): CompileTimeCompatible[A, A] =
        Unsafe(a, a)

      override def trans[A, B, C](f: CompileTimeCompatible[A, B], g: CompileTimeCompatible[B, C]): CompileTimeCompatible[A, C] =
        (f, g) match
          case (Unsafe(a, _), Unsafe(_, c)) => Unsafe(a, c)

      override def symm[A, B](f: CompileTimeCompatible[A, B]): CompileTimeCompatible[B, A] =
        f match
          case Unsafe(a, b) => Unsafe(b, a)

      extension [A, B](f: CompileTimeCompatible[A, B]) {
        override def apply(using Quotes): Expr[A] => Expr[B] =
          f match
            case Unsafe(a, b) =>
              given Type[B] = b
              a => a.asExprOf[B]

        override def unapply(using Quotes): Expr[B] => Expr[A] =
          symm(f).apply

        override def lift[M[_]](using Quotes, Type[M]): CompileTimeCompatible[M[A], M[B]] =
          f match
            case Unsafe(a, b) =>
              given Type[A] = a
              given Type[B] = b
              Unsafe(Type.of[M[A]], Type.of[M[B]])
      }

      override def biLift[A, B, X, Y](
        r: CompileTimeCompatible[A, X],
        s: CompileTimeCompatible[B, Y],
      )[H[_, _]](using
        Quotes,
        Type[H],
      ): CompileTimeCompatible[H[A, B], H[X, Y]] =
        (r, s) match
          case (Unsafe(a, x), Unsafe(b, y)) =>
            given Type[A] = a
            given Type[B] = b
            given Type[X] = x
            given Type[Y] = y
            Unsafe(Type.of[H[A, B]], Type.of[H[X, Y]])
    }
  }

  sealed trait RuntimeCoercible[A, B] {
    def l: Type[A]
    def r: Type[B]
  }

  object RuntimeCoercible {
    private case class Unsafe[A, B](l: Type[A], r: Type[B]) extends RuntimeCoercible[A, B]

    def unsafeAssert[A, B](using a: Type[A], b: Type[B]): RuntimeCoercible[A, B] =
      new Unsafe(a, b)

    given Quotes => Compatible[RuntimeCoercible] {
      import jing.openapi.model.*
      import libretto.lambda.util.SingletonType

      override def lift_||[A, B, X, Y](aRb: A `RuntimeCoercible` B, xRy: X `RuntimeCoercible` Y): (A || X) `RuntimeCoercible` (B || Y) = ???
      override def lift_enm[A, B, Cs, Ds](aRb: A `RuntimeCoercible` B, cRd: Cs `RuntimeCoercible` Ds): Enum[A, Cs] `RuntimeCoercible` Enum[B, Ds] = ???

      extension [A, B](aRb: A `RuntimeCoercible` B) {
        override def lift_const: Const[A] `RuntimeCoercible` Const[B] = ???

        override def lift_arr: Arr[A] `RuntimeCoercible` Arr[B] =
          given Type[A] = aRb.l
          given Type[B] = aRb.r
          unsafeAssert

        override def lift_obj: Obj[A] `RuntimeCoercible` Obj[B] =
          given Type[A] = aRb.l
          given Type[B] = aRb.r
          unsafeAssert

        override def lift_discriminatedUnion: DiscriminatedUnion[A] `RuntimeCoercible` DiscriminatedUnion[B] = ???
        override def lift_-::-[Lbl <: String](using SingletonType[Lbl]): (Lbl :: A) `RuntimeCoercible` (Lbl :: B) = ???
        override def lift_-:?-[Lbl <: String](using SingletonType[Lbl]): (Lbl :? A) `RuntimeCoercible` (Lbl :? B) = ???
        override def lift_oops: Oops[A] `RuntimeCoercible` Oops[B] = ???
      }

      override def lift_void: Void `RuntimeCoercible` Void = unsafeAssert
      override def lift_int32: Int32 `RuntimeCoercible` Int32 = ???
      override def lift_int64: Int64 `RuntimeCoercible` Int64 = ???
      override def lift_str: Str `RuntimeCoercible` Str = unsafeAssert[Str, Str]
      override def lift_bool: Bool `RuntimeCoercible` Bool = ???
      override def lift_singletonInt[T <: Int](t: SingletonType[T]): T `RuntimeCoercible` T = ???
      override def lift_singletonLong[T <: Long](t: SingletonType[T]): T `RuntimeCoercible` T = ???
      override def lift_singletonString[T <: String](t: SingletonType[T]): T `RuntimeCoercible` T = ???
      override def lift_singletonBoolean[T <: Boolean](t: SingletonType[T]): T `RuntimeCoercible` T = ???
    }

    given ExprIso[RuntimeCoercible] {
      override def refl[A](using a: Type[A]): RuntimeCoercible[A, A] =
        Unsafe(a, a)

      override def trans[A, B, C](r: RuntimeCoercible[A, B], s: RuntimeCoercible[B, C]): RuntimeCoercible[A, C] =
        (r, s) match
          case (Unsafe(a, _), Unsafe(_, c)) => Unsafe(a, c)

      override def symm[A, B](f: RuntimeCoercible[A, B]): RuntimeCoercible[B, A] =
        f match
          case Unsafe(a, b) => Unsafe(b, a)

      extension [A, B](f: RuntimeCoercible[A, B]) {
        override def apply(using Quotes): Expr[A] => Expr[B] =
          f match
            case Unsafe(a, b) =>
              given Type[A] = a
              given Type[B] = b
              a => '{ $a.asInstanceOf[B] }

        override def unapply(using Quotes): Expr[B] => Expr[A] =
          symm(f).apply

        override def lift[M[_]](using Quotes, Type[M]): RuntimeCoercible[M[A], M[B]] =
          f match
            case Unsafe(a, b) =>
              given Type[A] = a
              given Type[B] = b
              Unsafe(Type.of[M[A]], Type.of[M[B]])
      }

      override def biLift[A, B, X, Y](
        r: RuntimeCoercible[A, X],
        s: RuntimeCoercible[B, Y],
      )[H[_, _]](using
        Quotes,
        Type[H],
      ): RuntimeCoercible[H[A, B], H[X, Y]] =
        (r, s) match
          case (Unsafe(a, x), Unsafe(b, y)) =>
            given Type[A] = a
            given Type[B] = b
            given Type[X] = x
            given Type[Y] = y
            Unsafe(Type.of[H[A, B]], Type.of[H[X, Y]])
    }
  }
}
