package jing.openapi.model

import libretto.lambda.util.SingletonType

sealed trait ScalaValueOf[V, T] {
  import ScalaValueOf.*

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
}