package jing.openapi.examples.petstore.server

import cats.data.{State, StateT}
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import jing.openapi.examples.petstore.api.schemas.*
import jing.openapi.examples.petstore.model
import jing.openapi.examples.petstore.server.InMemoryPetstore.PetstoreState
import jing.openapi.model.Value

class InMemoryPetstore private(state: Ref[IO, PetstoreState]) {
  def createPet(pet: Value[Pet]): IO[Either[String, Value[Pet]]] =
    model.PetIn.fromApi(pet) match
      case Left(msg) =>
        Left(msg).pure[IO]
      case Right(petIn) =>
        state
          .modifyState(InMemoryPetstore.createPet(petIn).toState)
          .map(_.map(pet => pet.toApi))
}

object InMemoryPetstore {
  def initialize: IO[InMemoryPetstore] =
    Ref[IO]
      .of(PetstoreState.empty)
      .map(InMemoryPetstore(_))

  case class PetstoreState(
    nextId: Long,
    pets: Map[Long, model.Pet],
    categories: Map[Long, model.Category],
    tags: Map[Long, model.Tag],
  )

  object PetstoreState {
    def empty: PetstoreState =
      PetstoreState(1L, Map.empty, Map.empty, Map.empty)
  }

  val nextId: State[PetstoreState, Long] =
    State { s => (s.copy(nextId = s.nextId + 1), s.nextId) }

  val nextIdT: StateT[Either[String, _], PetstoreState, Long] =
    StateT.fromState(nextId.map(Right(_)))

  def getCategoryOpt(categoryId: Long): State[PetstoreState, Option[model.Category]] =
    ???

  def getCategory(categoryId: Long): StateT[Either[String, _], PetstoreState, model.Category] =
    StateT.inspectF:
      _.categories
        .get(categoryId)
        .toRight(left = s"Category id=$categoryId does not exist.")

  def getTag(tagId: Long): StateT[Either[String, _], PetstoreState, model.Tag] =
    StateT.inspectF:
      _.tags
        .get(tagId)
        .toRight(left = s"Tag id=$tagId does not exist.")

  def createPet(petIn: model.PetIn): StateT[Either[String, _], PetstoreState, model.Pet] =
    for {
      categoryOpt <- petIn.categoryId.traverse(getCategory)
      tags <- petIn.tagIds.traverse(getTag)
      id <- nextIdT
      pet = model.Pet(
        id = id,
        name = petIn.name,
        category = categoryOpt,
        photoUrls = petIn.photoUrls,
        tags = tags,
        status = petIn.status.getOrElse(model.PetStatus.Available),
      )
      _ <- setPetT(pet)
    } yield pet

  private def setPet(pet: model.Pet): State[PetstoreState, Unit] =
    State.modify { s => s.copy(pets = s.pets.updated(pet.id, pet)) }

  private def setPetT(pet: model.Pet): StateT[Either[String, _], PetstoreState, Unit] =
    StateT.fromState(setPet(pet).map(Right(_)))

  extension [E, S, A](st: StateT[Either[E, _], S, A]) {
    /** The new state is the original state in case of error. */
    def toState: State[S, Either[E, A]] =
      State(s0 => st.run(s0).fold(e => (s0, Left(e)), (s1, a) => (s1, Right(a))))
  }
}
