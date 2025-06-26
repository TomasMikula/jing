package jing.openapi.client.default

import jing.openapi.model.{DiscriminatedUnion, IsCaseOf, Value}

enum Response[T] {
  case Accurate(value: Value.Lenient[DiscriminatedUnion[T]])

  case WithExtraneousBody[Status, T](
    i: (Status IsCaseOf T) { type Type = Unit },
    body: Response.StringBody,
  ) extends Response[T]

  def statusCode: String =
    this match
      case Accurate(value) => value.discriminator
      case WithExtraneousBody(i, body) => i.label

  def show: String =
    this match
      case Accurate(value) => value.show
      case WithExtraneousBody(i, body) => s"${i.label}(${body.body})"
}

object Response {
  case class StringBody(
    contentType: Option[String],
    body: String,
  )
}
