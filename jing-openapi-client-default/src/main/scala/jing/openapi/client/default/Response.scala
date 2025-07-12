package jing.openapi.client.default

import jing.openapi.model.{DiscriminatedUnion, IsCaseOf, ValueModule}

sealed trait Response[+Value[_], T] {
  def statusCode: String
  def show: String
}

object Response {

  case class Accurate[Value[_], T](
    value: Value[DiscriminatedUnion[T]],
  )(using
    ValueModule[Value],
  ) extends Response[Value, T] {

    override def statusCode: String =
      value.discriminator

    override def show: String =
      summon[ValueModule[Value]].show(value)

  }

  case class WithExtraneousBody[Status, T](
    i: (Status IsCaseOf T) { type Type = Unit },
    body: Response.StringBody,
  ) extends Response[Nothing, T] {

    override def statusCode: String = i.label

    override def show: String = s"${i.label}(${body.body})"

  }

  case class StringBody(
    contentType: Option[String],
    body: String,
  )
}
