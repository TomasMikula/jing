package jing.openapi.model

import libretto.lambda.Items1Named

enum BodySchema[A] {

  case EmptyBody extends BodySchema[Unit]

  case Variants[Cases](
    byMediaType: Items1Named.Product[||, ::, Schema, Cases],
  ) extends BodySchema[DiscriminatedUnion[Cases]]

}