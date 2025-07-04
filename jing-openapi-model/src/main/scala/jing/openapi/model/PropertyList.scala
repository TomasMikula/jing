package jing.openapi.model

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
    def readNamedTuple[F[_]](t: PropsToNamedTuple[F, Ps]): ObjectMotif[ValueMotif.Object.Payload[F], Ps] =
      ps.zipWithNamedTuple[F, ValueMotif.Object.Payload[F]](t)(
        [A] => (_, fa) => fa,
        [A] => (_, ofa) => ofa,
      )

    def readNamedTuple2[F[_]](t: NamedTuple[PropNamesTuple[Ps], PropTypesTuple[F, Ps]]): ObjectMotif[ValueMotif.Object.Payload[F], Ps] =
      ps.zipWithNamedTuple2[F, ValueMotif.Object.Payload[F]](t)(
        [A] => (_, fa) => fa,
        [A] => (_, ofa) => ofa,
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
