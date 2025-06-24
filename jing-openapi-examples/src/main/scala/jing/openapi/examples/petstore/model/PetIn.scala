package jing.openapi.examples.petstore.model

import cats.syntax.all.*
import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

final case class PetIn(
  name: String,
  categoryId: Option[Long],
  photoUrls: IArray[String],
  tags: Option[IArray[Tag]],
  status: Option[PetStatus],
)

object PetIn {
  def fromApi(p: Value[api.schemas.Pet]): Either[String, PetIn] =
    val api.schemas.Pet(obj) = p
    for
      categoryIdOpt <- obj.props["category"].traverse:
        Category.idFromApi(_).toRight("missing category id")
    yield
      PetIn(
        name       = obj.props["name"].stringValue,
        categoryId = categoryIdOpt,
        photoUrls  = obj.props["photoUrls"].asArray.map(_.stringValue),
        tags       = obj.props["tags"].map(_.asArray.map(Tag.fromApi)),
        status     = obj.props["status"].map(PetStatus.fromApi),
      )
}