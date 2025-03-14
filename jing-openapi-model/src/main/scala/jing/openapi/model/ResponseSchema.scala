package jing.openapi.model

import libretto.lambda.Items1Named

/** Schema of a HTTP response. */
case class ResponseSchema[A](
  byStatusCode: Items1Named.Product[||, ::, BodySchema, A],
)
