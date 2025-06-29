package jing.openapi.model

import ObjectMotif.Mod
import libretto.lambda.util.SingletonType
import libretto.lambda.util.Exists
import libretto.lambda.util.Applicative
import libretto.lambda.util.Exists.Indeed

sealed trait ObjectMotif[F[_ <: Mod, _], Props] {

  def translate[G[_ <: Mod, _]](
    f: [M <: Mod, A] => F[M, A] => G[M, A],
  ): ObjectMotif[G, Props]

  def wipeTranslate[H[_ <: Mod, _]](
    f: [M <: Mod, A] => F[M, A] => Exists[H[M, _]],
  ): Exists[[X] =>> ObjectMotif[H, X]]

  def wipeTranslateA[G[_], H[_ <: Mod, _]](
    f: [M <: Mod, A] => F[M, A] => G[Exists[H[M, _]]],
  )(using
    Applicative[G],
  ): G[Exists[[X] =>> ObjectMotif[H, X]]]

}

object ObjectMotif {
  enum Mod:
    case Required
    case Optional

  case class Empty[F[_ <: Mod, _]]() extends ObjectMotif[F, Void] {

    override def translate[G[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[M, A],
    ): ObjectMotif[G, Void] =
      Empty()

    override def wipeTranslate[H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => Exists[[X] =>> H[M, X]],
    ): Exists[[X] =>> ObjectMotif[H, X]] =
      Indeed(Empty())

    override def wipeTranslateA[G[_], H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[Exists[[X] =>> H[M, X]]],
    )(using
      G: Applicative[G],
    ): G[Exists[[X] =>> ObjectMotif[H, X]]] =
      G.pure(Indeed(Empty()))

  }

  sealed trait NonEmpty[F[_ <: Mod, _], Ps] extends ObjectMotif[F, Ps]

  case class Snoc[F[_ <: Mod, _], Init, PropName <: String, PropType](
    init: ObjectMotif[F, Init],
    pname: SingletonType[PropName],
    pval: F[Mod.Required.type, PropType],
  ) extends ObjectMotif.NonEmpty[F, Init || PropName :: PropType] {

    override def translate[G[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[M, A],
    ): ObjectMotif[G, Init || PropName :: PropType] =
      Snoc(init.translate(f), pname, f(pval))

    override def wipeTranslate[H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => Exists[[X] =>> H[M, X]],
    ): Exists[[X] =>> ObjectMotif[H, X]] =
      Indeed(Snoc(init.wipeTranslate(f).value, pname, f(pval).value))

    override def wipeTranslateA[G[_], H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[Exists[[X] =>> H[M, X]]],
    )(using
      G: Applicative[G],
    ): G[Exists[[X] =>> ObjectMotif[H, X]]] =
      G.map2(
        init.wipeTranslateA(f),
        f(pval),
      ): (init, pval) =>
        Indeed(Snoc(init.value, pname, pval.value))

  }

  case class SnocOpt[F[_ <: Mod, _], Init, PropName <: String, PropType](
    init: ObjectMotif[F, Init],
    pname: SingletonType[PropName],
    pval: F[Mod.Optional.type, PropType],
  ) extends ObjectMotif.NonEmpty[F, Init || PropName :? PropType] {

    override def translate[G[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[M, A],
    ): ObjectMotif[G, Init || PropName :? PropType] =
      SnocOpt(init.translate(f), pname, f(pval))

    override def wipeTranslate[H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => Exists[[X] =>> H[M, X]],
    ): Exists[[X] =>> ObjectMotif[H, X]] =
      Indeed(SnocOpt(init.wipeTranslate(f).value, pname, f(pval).value))

    override def wipeTranslateA[G[_], H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[Exists[[X] =>> H[M, X]]],
    )(using
      G: Applicative[G],
    ): G[Exists[[X] =>> ObjectMotif[H, X]]] =
      G.map2(
        init.wipeTranslateA(f),
        f(pval),
      ): (init, pval) =>
        Indeed(SnocOpt(init.value, pname, pval.value))

  }
}
