package jing.openapi.examples.petstore.model

import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

case class Pet(
  id: Long,
  name: String,
  category: Option[Category],
  photoUrls: IArray[String],
  tags: Option[IArray[Tag]],
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
          .setOpt("tags", tags.map(ts => Value.arr(ts.map(Tag.toApi))))
          .set("status", status.singletonStringValue)
    )
}
