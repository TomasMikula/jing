package jing.openapi.model

import jing.openapi.model.ObjectMotif.Mod.{Optional, Required}
import libretto.lambda.util.TypeEqK

/** Witnesses that `K` is a name of a property in the list of properties `Ps`.
 *
 * Examples:
 *
 *     IsPropertyOf["name",  Void || "name" :: Str || "value" :: Int32 || "desc" :? Str ] { type Type = Str;   type Modality[T] = T }
 *     IsPropertyOf["value", Void || "name" :: Str || "value" :: Int32 || "desc" :? Str ] { type Type = Int32; type Modality[T] = T }
 *     IsPropertyOf["desc",  Void || "name" :: Str || "value" :: Int32 || "desc" :? Str ] { type Type = Str;   type Modality[T] = Option[T] }
 *
 * @see [[IsCaseOf]] is an analogue for lists without optional elements, such as variants of a [[DiscriminatedUnion]].
 */
infix sealed trait IsPropertyOf[K, Ps] {
  import IsPropertyOf.*

  type Type
  type Modality[+_]
  type Mod <: ObjectMotif.Mod

  def switch[R](
    caseLastProp:    [init] => (Ps =:= (init || K :: Type), TypeEqK[Modality, [A] =>> A], Mod =:= Required.type) => R,
    caseOptLastProp: [init] => (Ps =:= (init || K :? Type), TypeEqK[Modality, Option], Mod =:= Optional.type) => R,
    caseInitProp:    [init, last] => (Ps =:= (init || last), IsPropertyOf.Aux[K, init, Type, Modality, Mod]) => R,
  ): R

  def propertiesNotVoid(using Ps =:= Void): Nothing

  def modAndModalityInterlocked: Either[
    (Mod =:= Required.type, TypeEqK[Modality, [x] =>> x]),
    (Mod =:= Optional.type, TypeEqK[Modality, Option]),
  ]
}

object IsPropertyOf {
  type Aux[K, Ps, T, M[+_], N <: ObjectMotif.Mod] =
    IsPropertyOf[K, Ps] { type Type = T; type Modality = M; type Mod = N }

  type IsRequiredPropertyOf[K, Ps] =
    IsPropertyOf[K, Ps] { type Modality[T] = T }

  type IsOptionalPropertyOf[K, Ps] =
    IsPropertyOf[K, Ps] { type Modality = Option }

  case class IsLastPropertyOf[Init, K, V]() extends IsPropertyOf[K, Init || K :: V] {
    override type Type = V
    override type Modality[A] = A
    override type Mod = ObjectMotif.Mod.Required.type

    override def switch[R](
      caseLastProp: [init] => ((Init || K :: V) =:= (init || K :: V), TypeEqK[[A] =>> A, [A] =>> A], Mod =:= Required.type) => R,
      caseOptLastProp: [init] => ((Init || K :: V) =:= (init || K :? V), TypeEqK[[A] =>> A, Option], Mod =:= Optional.type) => R,
      caseInitProp: [init, last] => ((Init || K :: V) =:= (init || last), IsPropertyOf.Aux[K, init, V, [A] =>> A, Required.type]) => R,
    ): R =
      caseLastProp[Init](summon, summon, summon)

    override def propertiesNotVoid(using (Init || K :: V) =:= Void): Nothing =
      ||.isNotVoid

    override def modAndModalityInterlocked: Either[
      (Required.type =:= Required.type, TypeEqK[[A] =>> A, [A] =>> A]),
      (Required.type =:= Optional.type, TypeEqK[[A] =>> A, Option]),
    ] =
      Left((summon, summon))
  }

  case class IsOptionalLastPropertyOf[Init, K, V]() extends IsPropertyOf[K, Init || K :? V] {
    override type Type = V
    override type Modality = Option
    override type Mod = ObjectMotif.Mod.Optional.type

    override def switch[R](
      caseLastProp: [init] => ((Init || K :? V) =:= (init || K :: V),TypeEqK[Option, [A] =>> A], Mod =:= Required.type) => R,
      caseOptLastProp: [init] => ((Init || K :? V) =:= (init || K :? V), TypeEqK[Option, Option], Mod =:= Optional.type) => R,
      caseInitProp: [init, last] => ((Init || K :? V) =:= (init || last), IsPropertyOf.Aux[K, init, V, Option, Optional.type]) => R,
    ): R =
      caseOptLastProp[Init](summon, summon, summon)

    override def propertiesNotVoid(using (Init || K :? V) =:= Void): Nothing =
      ||.isNotVoid

    override def modAndModalityInterlocked: Either[
      (Optional.type =:= Required.type, TypeEqK[Option, [x] =>> x]),
      (Optional.type =:= Optional.type, TypeEqK[Option, Option]),
    ] =
      Right((summon, summon))
  }

  case class IsInitPropertyOf[K, Init, Last, T, M[+_], N <: ObjectMotif.Mod](
    i: IsPropertyOf.Aux[K, Init, T, M, N],
  ) extends IsPropertyOf[K, Init || Last] {
    override type Type = i.Type
    override type Modality = i.Modality
    override type Mod = i.Mod

    override def switch[R](
      caseLastProp:    [init] => ((Init || Last) =:= (init || K :: T), TypeEqK[M, [A] =>> A], Mod =:= Required.type) => R,
      caseOptLastProp: [init] => ((Init || Last) =:= (init || K :? T), TypeEqK[M, Option], Mod =:= Optional.type) => R,
      caseInitProp:    [init, last] => ((Init || Last) =:= (init || last), IsPropertyOf.Aux[K, init, T, M, N]) => R,
    ): R =
      caseInitProp[Init, Last](summon, i)

    override def propertiesNotVoid(using (Init || Last) =:= Void): Nothing =
      ||.isNotVoid

    override def modAndModalityInterlocked: Either[
      (N =:= Required.type, TypeEqK[M, [x] =>> x]),
      (N =:= Optional.type, TypeEqK[M, Option]),
    ] =
      i.modAndModalityInterlocked
  }

  given isLastPropertyOf[Init, K, V]: (IsRequiredPropertyOf[K, Init || K :: V] { type Type = V }) =
    IsLastPropertyOf()

  given isOptionalLastPropertyOf[Init, K, V]: (IsOptionalPropertyOf[K, Init || K :? V] { type Type = V }) =
    IsOptionalLastPropertyOf()

  given isInitPropertyOf[K, Init, Last](using i: IsPropertyOf[K, Init]): IsPropertyOf.Aux[K, Init || Last, i.Type, i.Modality, i.Mod] =
    IsInitPropertyOf[K, Init, Last, i.Type, i.Modality, i.Mod](i)
}
