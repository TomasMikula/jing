package jing.openapi.model

import libretto.lambda.Items1Named

/** Schema of a HTTP response. */
sealed trait ResponseSchema[A]

object ResponseSchema {
  case class ByStatusCode[As](
    items: Items1Named.Product[||, ::, BodySchema, As],
  ) extends ResponseSchema[As]
}
