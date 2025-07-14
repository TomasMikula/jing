package jing.openapi.examples.petstore.model

import cats.data.Ior
import cats.syntax.all.*
import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

final case class PetIn(
  name: String,
  categoryId: Option[Long],
  photoUrls: IArray[String],
  tags: Option[List[Ior[Long, String]]],
  status: Option[PetStatus],
)

object PetIn {
  def fromApi(p: Value[api.schemas.Pet]): Either[String, PetIn] =
    val api.schemas.Pet(obj) = p
    val pet = obj.toNamedTuple()
    for
      categoryIdOpt <- pet.category.traverse:
        Category.idFromApi(_).toRight("missing category.id")
      tagIdIorNames <- pet.tags
        .traverse: tags =>
          tags
            .asArray
            .toList
            .zipWithIndex
            .traverse: (tag, i) =>
              Tag.idIorNameFromApi(tag)
                .leftMap(errMsg => s"tags[$i]: $errMsg")
    yield
      PetIn(
        name       = pet.name.stringValue,
        categoryId = categoryIdOpt,
        photoUrls  = pet.photoUrls.asArray.map(_.stringValue),
        tags       = tagIdIorNames,
        status     = pet.status.map(PetStatus.fromApi),
      )
}