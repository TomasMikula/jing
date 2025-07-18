package jing.openapi.examples.petstore.model

import jing.openapi.examples.petstore.api
import jing.openapi.model.Value
import jing.openapi.model.Value.arr

case class Pet(
  id: Long,
  name: String,
  category: Option[Category],
  photoUrls: IArray[String],
  tags: List[Tag],
  status: PetStatus,
) {
  def toApi: Value[api.schemas.Pet] =
    api.schemas.Pet(
      id = id,
      name = name,
      category = category.map(Category.toApi).getOrElse(None),
      photoUrls = arr(photoUrls*),
      tags = arr(tags.map(Tag.toApi)*),
      status = status.toApi,
    )
}

object Pet {
  def idFromApi(pet: Value[api.schemas.Pet]): Option[Long] =
    ???
}
