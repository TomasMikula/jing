package jing.openapi.examples.petstore.model

import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

final case class Tag(
  id: Long,
  name: String,
)

object Tag {
  def idFromApi(tag: Value[api.schemas.Tag]): Option[Long] =
    val api.schemas.Tag(obj) = tag
    obj.props["id"].map(_.longValue)

  def toApi(tag: Tag): Value[api.schemas.Tag] =
    ???
}