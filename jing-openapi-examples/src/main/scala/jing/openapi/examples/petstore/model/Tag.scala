package jing.openapi.examples.petstore.model

import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

final case class Tag(
  id: Long,
  name: String,
)

object Tag {
  def fromApi(tag: Value[api.schemas.Tag]): Tag =
    ???

  def toApi(tag: Tag): Value[api.schemas.Tag] =
    ???
}