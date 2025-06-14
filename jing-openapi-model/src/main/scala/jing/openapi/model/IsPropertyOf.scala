package jing.openapi.model

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
sealed trait IsPropertyOf[K, Ps] {
  type Type
  type Modality[+_]

  def switch[R](
    caseLastProp:    [init] => (Ps =:= (init || K :: Type), TypeEqK[Modality, [A] =>> A]) => R,
    caseOptLastProp: [init] => (Ps =:= (init || K :? Type), TypeEqK[Modality, Option]) => R,
    caseInitProp:    [init, last] => (Ps =:= (init || last), IsPropertyOf.Aux[K, init, Type, Modality]) => R,
  ): R

  def propertiesNotVoid(using Ps =:= Void): Nothing
}

object IsPropertyOf {
  type Aux[K, Ps, T, M[+_]] =
    IsPropertyOf[K, Ps] { type Type = T; type Modality = M }

  type IsRequiredPropertyOf[K, Ps] =
    IsPropertyOf[K, Ps] { type Modality[T] = T }

  type IsOptionalPropertyOf[K, Ps] =
    IsPropertyOf[K, Ps] { type Modality = Option }

  case class IsLastPropertyOf[Init, K, V]() extends IsPropertyOf[K, Init || K :: V] {
    override type Type = V
    override type Modality[A] = A

    override def switch[R](
      caseLastProp: [init] => ((Init || K :: V) =:= (init || K :: V), TypeEqK[[A] =>> A, [A] =>> A]) => R,
      caseOptLastProp: [init] => ((Init || K :: V) =:= (init || K :? V), TypeEqK[[A] =>> A, Option]) => R,
      caseInitProp: [init, last] => ((Init || K :: V) =:= (init || last), IsPropertyOf.Aux[K, init, V, [A] =>> A]) => R,
    ): R =
      caseLastProp[Init](summon, summon)

    override def propertiesNotVoid(using (Init || K :: V) =:= Void): Nothing =
      ||.isNotVoid
  }

  case class IsOptionalLastPropertyOf[Init, K, V]() extends IsPropertyOf[K, Init || K :? V] {
    override type Type = V
    override type Modality = Option

    override def switch[R](
      caseLastProp: [init] => ((Init || K :? V) =:= (init || K :: V),TypeEqK[Option, [A] =>> A]) => R,
      caseOptLastProp: [init] => ((Init || K :? V) =:= (init || K :? V), TypeEqK[Option, Option]) => R,
      caseInitProp: [init, last] => ((Init || K :? V) =:= (init || last), IsPropertyOf.Aux[K, init, V, Option]) => R,
    ): R =
      caseOptLastProp[Init](summon, summon)

    override def propertiesNotVoid(using (Init || K :? V) =:= Void): Nothing =
      ||.isNotVoid
  }

  case class IsInitPropertyOf[K, Init, Last, T, M[+_]](
    i: IsPropertyOf.Aux[K, Init, T, M],
  ) extends IsPropertyOf[K, Init || Last] {
    override type Type = i.Type
    override type Modality = i.Modality

    override def switch[R](
      caseLastProp:    [init] => ((Init || Last) =:= (init || K :: T), TypeEqK[M, [A] =>> A]) => R,
      caseOptLastProp: [init] => ((Init || Last) =:= (init || K :? T), TypeEqK[M, Option]) => R,
      caseInitProp:    [init, last] => ((Init || Last) =:= (init || last), IsPropertyOf.Aux[K, init, T, M]) => R,
    ): R =
      caseInitProp[Init, Last](summon, i)

    override def propertiesNotVoid(using (Init || Last) =:= Void): Nothing =
      ||.isNotVoid
  }

  given isLastPropertyOf[Init, K, V]: (IsRequiredPropertyOf[K, Init || K :: V] { type Type = V }) =
    IsLastPropertyOf()

  given isOptionalLastPropertyOf[Init, K, V]: (IsOptionalPropertyOf[K, Init || K :? V] { type Type = V }) =
    IsOptionalLastPropertyOf()

  given isInitPropertyOf[K, Init, Last](using i: IsPropertyOf[K, Init]): IsPropertyOf.Aux[K, Init || Last, i.Type, i.Modality] =
    IsInitPropertyOf[K, Init, Last, i.Type, i.Modality](i)
}
