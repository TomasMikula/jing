package jing.openapi.examples.petstore.model

import cats.data.Ior
import cats.syntax.all.*
import jing.openapi.examples.petstore.api
import jing.openapi.model.Value

final case class PetIn(
  name: String,
  category: Option[Ior[Long, String]],
  photoUrls: IArray[String],
  tags: Option[List[Ior[Long, String]]],
  status: Option[PetStatus],
)

object PetIn {
  def fromApi(p: Value[api.schemas.Pet]): Either[String, PetIn] =
    val api.schemas.Pet(pet) = p
    for
      categoryOpt <- pet.category.traverse:
        Category.idIorNameFromApi(_)
          .leftMap(errMsg => s"category: $errMsg")
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
        category   = categoryOpt,
        photoUrls  = pet.photoUrls.asArray.map(_.stringValue),
        tags       = tagIdIorNames,
        status     = pet.status.map(PetStatus.fromApi),
      )
}