package jing.openapi.examples.petstore.model

import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

case class Category(
  id: Long,
  name: String,
)

object Category {
  def idFromApi(cat: Value[api.schemas.Category]): Option[Long] =
    val api.schemas.Category(obj) = cat
    obj.props["id"].map(_.longValue)

  def toApi(cat: Category): Value[api.schemas.Category] =
    ???
}
