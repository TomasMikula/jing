package jing.openapi.examples.petstore.model

import cats.data.Ior
import jing.openapi.examples.petstore.api
import jing.openapi.model.Value
import jing.openapi.model.Value.Obj

final case class Tag(
  id: Long,
  name: String,
)

object Tag {
  def idIorNameFromApi(tag: Value[api.schemas.Tag]): Either[String, Ior[Long, String]] =
    val api.schemas.Tag(Obj((id = idOpt, name = nameOpt))) = tag
    Ior.fromOptions(
      idOpt.map(_.longValue),
      nameOpt.map(_.stringValue)
    ).toRight("missing both 'id' and 'name'")

  def toApi(tag: Tag): Value[api.schemas.Tag] =
    api.schemas.Tag(id = tag.id, name = tag.name)
}