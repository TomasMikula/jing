package jing.openapi.examples.petstore.model

import cats.data.Ior
import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

final case class Tag(
  id: Long,
  name: String,
)

object Tag {
  def idIorNameFromApi(tag: Value[api.schemas.Tag]): Either[String, Ior[Long, String]] =
    val api.schemas.Tag(obj) = tag
    val props = obj.toNamedTuple()
    Ior.fromOptions(
      props.id.map(_.longValue),
      props.name.map(_.stringValue)
    ).toRight("missing both 'id' and 'name'")

  def toApi(tag: Tag): Value[api.schemas.Tag] =
    api.schemas.Tag(Value.obj(_(id = tag.id, name = tag.name)))
}