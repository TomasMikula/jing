package jing.openapi.client.default

import jing.openapi.model.{DiscriminatedUnion, DiscriminatorOf, IsCaseOf, ValueModule}

sealed trait Response[Value[_], T] {
  import Response.*

  def statusCode: String & DiscriminatorOf[T]
  def show: String

  /** Allows to assert a certain response status.
   *
   * Use only after you have inspected the [[statusCode]], or for happy path exploration.
   */
  def assertStatus: AssertStatus[Value, T, DiscriminatorOf[T]] =
    AssertStatus[Value, T, DiscriminatorOf[T]](this)
}

object Response {

  case class Accurate[Value[_], T](
    value: Value[DiscriminatedUnion[T]],
  )(using
    ValueModule[Value],
  ) extends Response[Value, T] {

    override def statusCode: String & DiscriminatorOf[T] =
      value.discriminator

    override def show: String =
      summon[ValueModule[Value]].show(value)

  }

  case class WithExtraneousBody[Status, Value[_], T](
    i: (Status IsCaseOf T) { type Type = Unit },
    body: Response.StringBody,
  ) extends Response[Value, T] {

    override def statusCode: String & DiscriminatorOf[T] =
      DiscriminatorOf.from[Status, T](i)

    override def show: String = s"${i.label}(${body.body})"

  }

  case class StringBody(
    contentType: Option[String],
    body: String,
  )

  class AssertStatus[Value[_], T, Discriminators <: DiscriminatorOf[T]](
    resp: Response[Value, T],
  ):
    /** Asserts that the response status is the given one (specified as type argument).
     *
     * @throws IllegalStateException if the status code is different from the given one.
     */
    def apply[S <: Discriminators](using i: S IsCaseOf T, V: ValueModule[Value]): Value[i.Type] =
      resp match
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
