package jing.openapi.examples.petstore.model

import jing.openapi.model.{Enum, Str, Value, ||}

enum PetStatus {
  case Available
  case Pending
  case Sold

  def singletonStringValue: "available" | "pending" | "sold" =
    this match
      case Available => "available"
      case Pending   => "pending"
      case Sold      => "sold"

}

object PetStatus {
  def fromApi(status: Value[Enum[Str, Void || "available" || "pending" || "sold"]]): PetStatus =
    ???

  def toApi(status: PetStatus): Value[Enum[Str, Void || "available" || "pending" || "sold"]] =
    ???
}
