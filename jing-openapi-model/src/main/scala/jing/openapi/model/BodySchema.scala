package jing.openapi.model

import libretto.lambda.Items1Named

sealed trait BodySchema[A]

object BodySchema {
  case object Empty extends BodySchema[Unit]

  sealed trait NonEmpty[A] extends BodySchema[A]

  case class Variants[Cases](
    byMediaType: Items1Named.Product[||, ::, Schema, Cases],
  ) extends BodySchema.NonEmpty[DiscriminatedUnion[Cases]]
}