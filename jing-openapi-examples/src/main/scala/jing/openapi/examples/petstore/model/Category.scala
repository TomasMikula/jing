package jing.openapi.examples.petstore.model

import cats.data.Ior
import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

case class Category(
  id: Long,
  name: String,
)

object Category {
  def idIorNameFromApi(cat: Value[api.schemas.Category]): Either[String, Ior[Long, String]] =
    val api.schemas.Category((id = id, name = name)) = cat
    Ior.fromOptions(
      id.map(_.longValue),
      name.map(_.stringValue)
    ).toRight("missing both 'id' and 'name'")

  def toApi(cat: Category): Value[api.schemas.Category] =
    api.schemas.Category(id = cat.id, name = cat.name)
}
