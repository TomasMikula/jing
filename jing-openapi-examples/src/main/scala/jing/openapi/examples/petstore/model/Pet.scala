package jing.openapi.examples.petstore.model

import jing.openapi.model.Value
import jing.openapi.examples.petstore.api

case class Pet(
  id: Long,
  name: String,
  category: Option[Category],
  photoUrls: Array[String],
  tags: Option[Array[Tag]],
  status: PetStatus,
) {
  def toApi: Value[api.schemas.Pet] =
    ???
}
