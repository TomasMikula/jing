package jing.openapi.model

import libretto.lambda.util.SingletonType

sealed trait ScalaValueOf[V, T] {
  import ScalaValueOf.*

  def get: V & ScalaReprOf[T] =
    this match
      case I32(value) => value.value
      case I64(value) => value.value
      case S(value) => value.value
      case B(value) => value.value

  def contains(that: ScalaReprOf[T]): Option[ScalaValueOf[that.type & V, T]] =
    this match
      case I32(value) =>
        value.value match
          case x: that.type => Some(I32(SingletonType(x)))
          case _ => None
      case I64(value) =>
        value.value match
          case x: that.type => Some(I64(SingletonType(x)))
          case _ => None
      case S(value) =>
        value.value match
          case x: that.type => Some(S(SingletonType(x)))
          case _ => None
      case B(value) =>
        value.value match
          case x: that.type => Some(B(SingletonType(x)))
          case _ => None

  def show: String =
    this match
      case I32(value) => (value.value: Int).toString
      case I64(value) => (value.value: Long).toString
      case S(value) => value.value
      case B(value) => (value.value: Boolean).toString
}

object ScalaValueOf {
  case class I32[I <: Int](value: SingletonType[I]) extends ScalaValueOf[I, Int32]
  case class I64[I <: Long](value: SingletonType[I]) extends ScalaValueOf[I, Int64]
  case class S[S <: String](value: SingletonType[S]) extends ScalaValueOf[S, Str]
  case class B[B <: Boolean](value: SingletonType[B]) extends ScalaValueOf[B, Bool]

  def i32(i: Int): ScalaValueOf[i.type, Int32] =
    I32(SingletonType(i))

  def i64(i: Long): ScalaValueOf[i.type, Int64] =
    I64(SingletonType(i))

  def str(s: String): ScalaValueOf[s.type, Str] =
    S(SingletonType(s))

  def bool(b: Boolean): ScalaValueOf[b.type, Bool] =
    B(SingletonType(b))

  def apply[T, D](t: T)(using q: (T QualifiesAs D)): ScalaValueOf[t.type, D] =
    q match
      case QualifiesAs.S => str(t)
      case QualifiesAs.I => i32(t)
      case QualifiesAs.L => i64(t)
      case QualifiesAs.B => bool(t)

  given [I <: Int] => (ev: SingletonType[I]) => ScalaValueOf[I, Int32] =
    I32(ev)

  given [I <: Long] => (ev: SingletonType[I]) => ScalaValueOf[I, Int64] =
    I64(ev)

  given [B <: Boolean] => (ev: SingletonType[B]) => ScalaValueOf[B, Bool] =
    B(ev)

  given [S <: String] => (ev: SingletonType[S]) => ScalaValueOf[S, Str] =
    S(ev)
}