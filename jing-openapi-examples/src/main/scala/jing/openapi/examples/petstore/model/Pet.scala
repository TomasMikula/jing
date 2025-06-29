package jing.openapi.examples.petstore.model

import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

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
      Value.obj:
        _
          .set("id", id)
          .set("name", name)
          .setOpt("category", category.map(Category.toApi))
          .set("photoUrls", Value.arr(photoUrls*))
          .set("tags", Value.arr(tags.map(Tag.toApi)*))
          .set("status", status.singletonStringValue)
    )
}

object Pet {
  def idFromApi(pet: Value[api.schemas.Pet]): Option[Long] =
    ???
}
