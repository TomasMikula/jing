package jing.openapi.client.default

import jing.openapi.model.{DiscriminatedUnion, DiscriminatorOf, IsCaseOf, ValueModule}

// `SC` (status code) is always instantiated to exactly `DiscriminatorOf[T]`,
// but having it as an explicit type parameter helps to reduce
// the `DiscriminatorOf[T]` match type to a union of singletons in IDE suggestions.
sealed trait Response[Value[_], T, SC <: DiscriminatorOf[T]] {
  import Response.*

  def statusCode: SC & String
  def show: String

  /** Asserts that the response status is the given one (specified as type argument).
   *
   * Use only after you have inspected the [[statusCode]], or for happy path exploration.
   *
   * @throws IllegalStateException if the status code is different from the given one.
   */
  def assertStatus[S <: SC](using i: S IsCaseOf T, V: ValueModule[Value]): Value[i.Type] =
    this match
      case Accurate(value) =>
        value.assertCase[S]
      case WithExtraneousBody(j, body) =>
        (IsCaseOf.toMember(i) testEqual IsCaseOf.toMember(j)) match
          case Some(ev) =>
            (ev: i.Type =:= Unit).flip.substituteCo[Value]:
              V.unit
          case None =>
            if (i.label == j.label)
              throw IllegalStateException(s"Seems like the same status ${i.label} is specified twice in the response schema")
            else
              throw IllegalStateException(s"Expected status ${i.label}, was ${j.label}")
}

object Response {

  case class Accurate[Value[_], T](
    value: Value[DiscriminatedUnion[T]],
  )(using
    ValueModule[Value],
  ) extends Response[Value, T, DiscriminatorOf[T]] {

    override def statusCode: String & DiscriminatorOf[T] =
      value.discriminator

    override def show: String =
      summon[ValueModule[Value]].show(value)

  }

  case class WithExtraneousBody[Status, Value[_], T](
    i: (Status IsCaseOf T) { type Type = Unit },
    body: Response.StringBody,
  ) extends Response[Value, T, DiscriminatorOf[T]] {

    override def statusCode: String & DiscriminatorOf[T] =
      DiscriminatorOf.from[Status, T](i)

    override def show: String = s"${i.label}(${body.body})"

  }

  case class StringBody(
    contentType: Option[String],
    body: String,
  )

}
