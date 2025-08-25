package jing.openapi.examples.petstore.server

import cats.data.{Ior, State, StateT}
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import jing.openapi.examples.petstore.api.schemas.*
import jing.openapi.examples.petstore.model
import jing.openapi.examples.petstore.server.InMemoryPetstore.PetstoreState
import jing.openapi.model.{||, Arr, Enum, Str, Value}
import jing.openapi.examples.petstore.model.PetStatus

class InMemoryPetstore private(state: Ref[IO, PetstoreState]) {
  def createPet(pet: Value[Pet]): IO[Either[String, Value[Pet]]] =
    model.PetIn.fromApi(pet) match
      case Left(msg) =>
        Left(msg).pure[IO]
      case Right(petIn) =>
        createPet(petIn)
          .map(_.map(pet => pet.toApi))

  def createPet(petIn: model.PetIn): IO[Either[String, model.Pet]] =
    state.modifyState(InMemoryPetstore.createPet(petIn).toState)

  def updatePet(pet: Value[Pet]): IO[Either[String, Value[Pet]]] =
    (
      model.Pet.idFromApi(pet).toRight("Missing pet.id"),
      model.PetIn.fromApi(pet),
    ).parTupled match
      case Left(errMsg) =>
        Left(errMsg).pure[IO]
      case Right((petId, petIn)) =>
        state
          .modifyState(InMemoryPetstore.updatePet(petId, petIn).toState)
          .map(_.map(pet => pet.toApi))

  def updateNameAndStatus(petId: Long, newName: Option[String], newStatus: Option[String]): IO[Either[String, Value[Pet]]] =
    newStatus
      .traverse[Either[String, _], PetStatus](PetStatus.fromString(_))
      .flatTraverse: (newStatus: Option[PetStatus]) =>
        state
          .modifyState(InMemoryPetstore.updateNameAndStatus(petId, newName, newStatus).toState)
          .map(_.map(pet => pet.toApi))

  def findAll: IO[Value[Arr[Pet]]] =
    state
      .get
      .map: state =>
        Value.arr(state.pets.values.map(_.toApi).toArray*)

  def findByStatus(
    status: Value[Enum[Str, Void || "available" || "pending" || "sold"]],
  ): IO[Value[Arr[Pet]]] =
    val st = PetStatus.fromApi(status)
    state
      .get
      .map: state =>
        Value.arr(state.pets.values.filter(_.status == st).map(_.toApi).toSeq*)

  def findByTags(
    tags: Value[Arr[Str]],
  ): IO[Value[Arr[Pet]]] =
    val tagNames = tags.asArray.map(_.stringValue).toSet
    state
      .get
      .map: state =>
        Value.arr:
          IArray.from:
            state.pets.values
              .filter(pet => pet.tags.exists(t => tagNames contains t.name))
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

  def getPet(petId: Long): StateT[Either[String, _], PetstoreState, model.Pet] =
    StateT.inspectF:
      _.pets
        .get(petId)
        .toRight(left = s"Pet id=$petId does not exist.")

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

  def getCategoryByNameOpt(name: String): StateT[Either[String, _], PetstoreState, Option[model.Category]] =
    StateT.inspectF:
      _.categories
        .values
        .find(_.name == name)
        .asRight

  def getTagByNameOpt(tagName: String): StateT[Either[String, _], PetstoreState, Option[model.Tag]] =
    StateT.inspectF:
      _.tags
        .values
        .find(_.name == tagName)
        .asRight

  private def createCategory(name: String): StateT[Either[String, _], PetstoreState, model.Category] =
    for
      id <- nextIdT
      cat = model.Category(id, name)
      _ <- setCategoryT(cat)
    yield
      cat

  private def createTag(tagName: String): StateT[Either[String, _], PetstoreState, model.Tag] =
    for
      id <- nextIdT
      tag = model.Tag(id, tagName)
      _ <- setTagT(tag)
    yield
      tag

  def getOrCreateCategoryByName(name: String): StateT[Either[String, _], PetstoreState, model.Category] =
    getCategoryByNameOpt(name)
      .flatMap:
        case Some(cat) => cat.pure
        case None => createCategory(name)

  def getOrCreateTagByName(tagName: String): StateT[Either[String, _], PetstoreState, model.Tag] =
    getTagByNameOpt(tagName)
      .flatMap:
        case Some(tag) => tag.pure
        case None => createTag(tagName)

  def getOrCreateCategory(
    catIdIorName: Ior[Long, String],
  ): StateT[Either[String, _], PetstoreState, model.Category] =
    catIdIorName match
      case Ior.Left(id) =>
        getCategory(id)
      case Ior.Right(name) =>
        getOrCreateCategoryByName(name)
      case Ior.Both(id, name) =>
        getCategory(id)
          .flatMapF: cat =>
            if (cat.name == name)
              then Right(cat)
              else Left(s"Category id=$id is not named '$name'. It is named '${cat.name}'.")

  def getOrCreateTag(
    tagIdIorName: Ior[Long, String],
  ): StateT[Either[String, _], PetstoreState, model.Tag] =
    tagIdIorName match
      case Ior.Left(id) =>
        getTag(id)
      case Ior.Right(name) =>
        getOrCreateTagByName(name)
      case Ior.Both(id, name) =>
        getTag(id)
          .flatMapF: tag =>
            if (tag.name == name)
              then Right(tag)
              else Left(s"Tag id=$id is not named '$name'. It is named '${tag.name}'.")

  def createPet(petIn: model.PetIn): StateT[Either[String, _], PetstoreState, model.Pet] =
    for
      categoryOpt <- petIn.category.traverse(getOrCreateCategory)
      tagsOpt <- petIn.tags.traverse(_.traverse(getOrCreateTag))
      id <- nextIdT
      pet = model.Pet(
        id = id,
        name = petIn.name,
        category = categoryOpt,
        photoUrls = petIn.photoUrls,
        tags = tagsOpt.getOrElse(Nil),
        status = petIn.status.getOrElse(model.PetStatus.Available),
      )
      _ <- setPetT(pet)
    yield
      pet

  def updatePet(petId: Long, petIn: model.PetIn): StateT[Either[String, _], PetstoreState, model.Pet] =
    val model.PetIn(name, categoryIdOpt, photoUrls, tags, statusOpt) = petIn
    for
      pet0 <- getPet(petId)
      categoryOpt <- categoryIdOpt.traverse(getOrCreateCategory)
      tagsOpt <- tags.traverse(_.traverse(getOrCreateTag))
    yield
      pet0.copy(
        name = petIn.name,
        category = categoryOpt.orElse(pet0.category),
        photoUrls = petIn.photoUrls,
        tags = tagsOpt.getOrElse(pet0.tags),
        status = statusOpt.getOrElse(pet0.status),
      )

  def updateNameAndStatus(
    petId: Long,
    newName: Option[String],
    newStatus: Option[PetStatus],
  ): StateT[Either[String, _], PetstoreState, model.Pet] =
    for
      pet0 <- getPet(petId)
    yield
      pet0.copy(
        name = newName.getOrElse(pet0.name),
        status = newStatus.getOrElse(pet0.status)
      )

  private def setPet(pet: model.Pet): State[PetstoreState, Unit] =
    State.modify { s => s.copy(pets = s.pets.updated(pet.id, pet)) }

  private def setPetT(pet: model.Pet): StateT[Either[String, _], PetstoreState, Unit] =
    StateT.fromState(setPet(pet).map(Right(_)))

  private def setCategoryT(cat: model.Category): StateT[Either[String, _], PetstoreState, Unit] =
    StateT.modify { s => s.copy(categories = s.categories.updated(cat.id, cat)) }

  private def setTagT(tag: model.Tag): StateT[Either[String, _], PetstoreState, Unit] =
    StateT.modify { s => s.copy(tags = s.tags.updated(tag.id, tag)) }

  extension [E, S, A](st: StateT[Either[E, _], S, A]) {
    /** The new state is the original state in case of error. */
    def toState: State[S, Either[E, A]] =
      State(s0 => st.run(s0).fold(e => (s0, Left(e)), (s1, a) => (s1, Right(a))))
  }
}
