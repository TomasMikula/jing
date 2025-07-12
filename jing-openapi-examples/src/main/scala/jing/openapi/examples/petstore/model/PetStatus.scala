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
    val statusStr: "available" | "pending" | "sold" = status.scalaValue
    statusStr match
      case "available" => Available
      case "pending" => Pending
      case "sold" => Sold

  def fromString(status: String): Either[String, PetStatus] =
    status match
      case "available" => Right(Available)
      case "pending" => Right(Pending)
      case "sold" => Right(Sold)
      case other => Left(s"Invalid status '$other'. Permitted values are ${PetStatus.values.map(_.singletonStringValue).mkString("'", "', '", "'")}.")

  def toApi(status: PetStatus): Value[Enum[Str, Void || "available" || "pending" || "sold"]] =
    ???
}
