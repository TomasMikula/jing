package jing.openapi.examples.petstore.model

import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

final case class PetIn(
  name: String,
  category: Option[Category],
  photoUrls: Array[String],
  tags: Option[Array[Tag]],
  status: Option[PetStatus],
)

object PetIn {
  def fromApi(p: Value[api.schemas.Pet]): PetIn =
    ???
}