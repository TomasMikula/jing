package jing.openapi.model

import libretto.lambda.util.SingletonType

/** Witnesses that the domain type `T` represents a singleton string schema. */
sealed trait SingletonStringSchema[T] {
  import SingletonStringSchema.*

  def stringValue: String =
    this match
      case Constant(value) => value.value
      case SingletonEnum(value) => value.value
}

object SingletonStringSchema {
  case class Constant[S <: String](value: SingletonType[S]) extends SingletonStringSchema[Const[S]]
  case class SingletonEnum[S <: String](value: SingletonType[S]) extends SingletonStringSchema[Enum[Str, Void || S]]
}
