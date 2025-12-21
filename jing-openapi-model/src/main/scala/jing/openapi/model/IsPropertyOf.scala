package jing.openapi.model

import libretto.lambda.util.TypeEqK

/** Witnesses that `K` is a name of a property in the list of properties `Ps`.
 *
 * Examples:
 *
 *     IsPropertyOf["name",  Void || "name" :: Str || "value" :: Int32 || "desc" :? Str ] { type Type = Str;   type ReqOrOpt[F[_], G[_]] = F }
 *     IsPropertyOf["value", Void || "name" :: Str || "value" :: Int32 || "desc" :? Str ] { type Type = Int32; type ReqOrOpt[F[_], G[_]] = F }
 *     IsPropertyOf["desc",  Void || "name" :: Str || "value" :: Int32 || "desc" :? Str ] { type Type = Str;   type ReqOrOpt[F[_], G[_]] = G }
 *
 * @see [[IsCaseOf]] is an analogue for lists without optional elements, such as variants of a [[DiscriminatedUnion]].
 */
infix sealed trait IsPropertyOf[K, Ps] {
  type Type
  type ReqOrOpt[Req[_], Opt[_]] <: [A] =>> Req[A] | Opt[A]

  def switch[R](
    caseLastProp:    [init] => (Ps =:= (init || K :: Type), [F[_], G[_]] => DummyImplicit ?=> TypeEqK[ReqOrOpt[F, G], F]) => R,
    caseOptLastProp: [init] => (Ps =:= (init || K :? Type), [F[_], G[_]] => DummyImplicit ?=> TypeEqK[ReqOrOpt[F, G], G]) => R,
    caseInitProp:    [init, last] => (Ps =:= (init || last), IsPropertyOf.Aux[K, init, Type, ReqOrOpt]) => R,
  ): R

  def propertiesNotVoid(using Ps =:= Void): Nothing

  def inInit[Last]: IsPropertyOf.Aux[K, Ps || Last, Type, ReqOrOpt] =
    IsPropertyOf.IsInitPropertyOf(this)
}

object IsPropertyOf {
  type Aux1[K, Ps, T] =
    IsPropertyOf[K, Ps] { type Type = T }

  type Aux[K, Ps, T, RoO <: [R[_], O[_]] =>> [A] =>> R[A] | O[A]] =
    IsPropertyOf[K, Ps] { type Type = T; type ReqOrOpt[F[_], G[_]] = RoO[F, G] }

  infix type IsRequiredPropertyOf[K, Ps] =
    IsPropertyOf[K, Ps] { type ReqOrOpt[F[_], G[_]] = F }

  object IsRequiredPropertyOf {
    type Aux[K, Ps, T] = IsRequiredPropertyOf[K, Ps] { type Type = T }
  }

  infix type IsOptionalPropertyOf[K, Ps] =
    IsPropertyOf[K, Ps] { type ReqOrOpt[F[_], G[_]] = G }

  object IsOptionalPropertyOf {
    type Aux[K, Ps, T] = IsOptionalPropertyOf[K, Ps] { type Type = T }
  }

  extension [K, Ps](i: IsRequiredPropertyOf[K, Ps]) {
    def switchReq[R](
      caseLastProp: [init] => (Ps =:= (init || K :: i.Type)) => R,
      caseInitProp: [init, last] => (Ps =:= (init || last), IsRequiredPropertyOf.Aux[K, init, i.Type]) => R,
    ): R =
      i.switch(
        caseLastProp = [init] => (ev1, _) => caseLastProp[init](ev1),
        caseOptLastProp = [init] => (_, _) => throw AssertionError("Impossible"),
        caseInitProp = [init, last] => (ev1, j) => caseInitProp(ev1, j),
      )
  }

  extension [K, Ps](i: IsOptionalPropertyOf[K, Ps]) {
    def switchOpt[R](
      caseLastProp: [init] => (Ps =:= (init || K :? i.Type)) => R,
      caseInitProp: [init, last] => (Ps =:= (init || last), IsOptionalPropertyOf.Aux[K, init, i.Type]) => R,
    ): R =
      i.switch(
        caseLastProp = [init] => (_, _) => throw AssertionError("Impossible"),
        caseOptLastProp = [init] => (ev1, _) => caseLastProp[init](ev1),
        caseInitProp = [init, last] => (ev1, j) => caseInitProp(ev1, j),
      )
  }

  case class IsRequiredLastPropertyOf[Init, K, V]() extends IsPropertyOf[K, Init || K :: V] {
    override type Type = V
    override type ReqOrOpt[Req[_], Opt[_]] = Req

    override def switch[R](
      caseLastProp: [init] => ((Init || K :: V) =:= (init || K :: V), [F[_], G[_]] => DummyImplicit ?=> TypeEqK[F, F]) => R,
      caseOptLastProp: [init] => ((Init || K :: V) =:= (init || K :? V), [F[_], G[_]] => DummyImplicit ?=> TypeEqK[F, G]) => R,
      caseInitProp: [init, last] => ((Init || K :: V) =:= (init || last), IsPropertyOf.Aux[K, init, V, [Req[_], Opt[_]] =>> Req]) => R,
    ): R =
      caseLastProp[Init](summon, [F[_], G[_]] => DummyImplicit ?=> summon)

    override def propertiesNotVoid(using (Init || K :: V) =:= Void): Nothing =
      ||.isNotVoid
  }

  case class IsOptionalLastPropertyOf[Init, K, V]() extends IsPropertyOf[K, Init || K :? V] {
    override type Type = V
    override type ReqOrOpt[Req[_], Opt[_]] = Opt

    override def switch[R](
      caseLastProp: [init] => ((Init || K :? V) =:= (init || K :: V), [F[_], G[_]] => DummyImplicit ?=> TypeEqK[G, F]) => R,
      caseOptLastProp: [init] => ((Init || K :? V) =:= (init || K :? V), [F[_], G[_]] => DummyImplicit ?=> TypeEqK[G, G]) => R,
      caseInitProp: [init, last] => ((Init || K :? V) =:= (init || last), IsPropertyOf.Aux[K, init, V, [Req[_], Opt[_]] =>> Opt]) => R,
    ): R =
      caseOptLastProp[Init](summon, [F[_], G[_]] => DummyImplicit ?=> summon)

    override def propertiesNotVoid(using (Init || K :? V) =:= Void): Nothing =
      ||.isNotVoid
  }

  case class IsInitPropertyOf[K, Init, Last, T, RoO <: [Req[_], Opt[_]] =>> [A] =>> Req[A] | Opt[A]](
    i: IsPropertyOf.Aux[K, Init, T, RoO],
  ) extends IsPropertyOf[K, Init || Last] {
    override type Type = i.Type
    override type ReqOrOpt[Req[_], Opt[_]] = i.ReqOrOpt[Req, Opt]

    override def switch[R](
      caseLastProp:    [init] => ((Init || Last) =:= (init || K :: T), [F[_], G[_]] => DummyImplicit ?=> TypeEqK[i.ReqOrOpt[F, G], F]) => R,
      caseOptLastProp: [init] => ((Init || Last) =:= (init || K :? T), [F[_], G[_]] => DummyImplicit ?=> TypeEqK[i.ReqOrOpt[F, G], G]) => R,
      caseInitProp:    [init, last] => ((Init || Last) =:= (init || last), IsPropertyOf.Aux[K, init, T, RoO]) => R,
    ): R =
      caseInitProp[Init, Last](summon, i)

    override def propertiesNotVoid(using (Init || Last) =:= Void): Nothing =
      ||.isNotVoid
  }

  given isRequiredLastPropertyOf[Init, K, V]: (IsRequiredPropertyOf[K, Init || K :: V] { type Type = V }) =
    IsRequiredLastPropertyOf()

  given isOptionalLastPropertyOf[Init, K, V]: (IsOptionalPropertyOf[K, Init || K :? V] { type Type = V }) =
    IsOptionalLastPropertyOf()

  given isInitPropertyOf[K, Init, Last](using i: IsPropertyOf[K, Init]): IsPropertyOf.Aux[K, Init || Last, i.Type, i.ReqOrOpt] =
    IsInitPropertyOf[K, Init, Last, i.Type, i.ReqOrOpt](i)
}
