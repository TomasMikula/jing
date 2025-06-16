package jing.openapi.examples.petstore.server

import cats.data.State
import cats.effect.{IO, Ref}
import jing.openapi.examples.petstore.api.schemas.*
import jing.openapi.examples.petstore.model
import jing.openapi.examples.petstore.server.InMemoryPetstore.PetstoreState
import jing.openapi.model.Value

class InMemoryPetstore private(state: Ref[IO, PetstoreState]) {
  def createPet(pet: Value[Pet]): IO[Value[Pet]] =
    val petIn = model.PetIn.fromApi(pet)
    state
      .modifyState(InMemoryPetstore.createPet(petIn))
      .map(_.toApi)
}

object InMemoryPetstore {
  def initialize: IO[InMemoryPetstore] =
    Ref[IO]
      .of(PetstoreState.empty)
      .map(InMemoryPetstore(_))

  case class PetstoreState(
    nextId: Long,
    pets: Map[Long, model.Pet],
  )

  object PetstoreState {
    def empty: PetstoreState =
      PetstoreState(1L, Map.empty)
  }

  val nextId: State[PetstoreState, Long] =
    State { s => (s.copy(nextId = s.nextId + 1), s.nextId) }

  def createPet(petIn: model.PetIn): State[PetstoreState, model.Pet] =
    for {
      id <- nextId
      pet = model.Pet(
        id = id,
        name = petIn.name,
        category = petIn.category,
        photoUrls = petIn.photoUrls,
        tags = petIn.tags,
        status = petIn.status.getOrElse(model.PetStatus.Available),
      )
      _ <- setPet(pet)
    } yield pet

  private def setPet(pet: model.Pet): State[PetstoreState, Unit] =
    State.modify { s => s.copy(pets = s.pets.updated(pet.id, pet)) }
}
