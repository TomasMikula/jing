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
    val api.schemas.Category(obj) = cat
    val props = obj.toNamedTuple()
    Ior.fromOptions(
      props.id.map(_.longValue),
      props.name.map(_.stringValue)
    ).toRight("missing both 'id' and 'name'")

  def toApi(cat: Category): Value[api.schemas.Category] =
    api.schemas.Category(Value.obj(_(id = cat.id, name = cat.name)))
}
