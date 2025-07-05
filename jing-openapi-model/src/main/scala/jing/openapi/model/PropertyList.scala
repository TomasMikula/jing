package jing.openapi.model

import jing.openapi.model.ObjectMotif.Mod
import jing.openapi.model.ObjectMotif.Mod.{Optional, Required}
import libretto.lambda.util.SingletonType

import scala.NamedTuple.NamedTuple
import scala.annotation.targetName

/** Witnesses that `Ps` is a list of object properties, i.e. of the form
 *
 * ```
 * "x" :: T1 || "y" :? T2 || ...
 * ```
 *
 * where `||` associates to the left.
 */
opaque type PropertyList[Ps] =
  ObjectMotif[[M <: ObjectMotif.Mod, A] =>> Unit, Ps]

object PropertyList {
  extension [Ps](ps: PropertyList[Ps]) {
    def readNamedTuple[F[_]](t: NamedTuple[PropNamesTuple[Ps], PropTypesTupleU[F, Ps]])[H[_ <: Mod, _]](
      fReq: [A] => F[A] => H[Required.type, A],
      fOpt: [A] => (F[A] | None.type) => H[Optional.type, A],
    ): ObjectMotif[H, Ps] =
      ps.zipWithNamedTuple[F, H](t)(
        [A] => (_, fa) => fReq(fa),
        [A] => (_, ofa) => fOpt(ofa),
      )
  }

  given PropertyList[Void] =
    ObjectMotif.Empty()

  given [Init, K <: String, V] => (
    init: PropertyList[Init],
    k: SingletonType[K],
  ) => PropertyList[Init || K :: V] =
    ObjectMotif.Snoc(init, k, ())

  @targetName("propListSnocOpt")
  given [Init, K <: String, V] => (
    init: PropertyList[Init],
    k: SingletonType[K],
  ) => PropertyList[Init || K :? V] =
    ObjectMotif.SnocOpt(init, k, ())
}
