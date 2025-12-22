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
  ObjectMotif[[_] =>> Unit, [_] =>> Unit, Ps]

object PropertyList {
  def from[F[_], G[_], Ps](obj: ObjectMotif[F, G, Ps]): PropertyList[Ps] =
    obj.translate[[_] =>> Unit, [_] =>> Unit](
      [A] => _ => (),
      [A] => _ => (),
    )

  extension [Ps](ps: PropertyList[Ps]) {
    def readNamedTuple[Req[_], Opt[_]](
      t: NamedTuple[PropNamesTuple[Ps], PropTypesTupleF[Req, Opt, Ps]],
    ): ObjectMotif[Req, Opt, Ps] =
      ps.zipWithNamedTuple[Req, Opt, Req, Opt](t)(
        [A] => (_, ra) => ra,
        [A] => (_, oa) => oa,
      )

    def readNamedTuple[Req[_], Opt[_], G[_], H[_]](
      t: NamedTuple[PropNamesTuple[Ps], PropTypesTupleF[Req, Opt, Ps]],
    )(
      g: [A] => Req[A] => G[A],
      h: [A] => Opt[A] => H[A],
    ): ObjectMotif[G, H, Ps] =
      ps.zipWithNamedTuple[Req, Opt, G, H](t)(
        [A] => (_, ra) => g(ra),
        [A] => (_, oa) => h(oa),
      )
  }

  given PropertyList[Void] =
    ObjectMotif.Empty()

  given [Init, K <: String, V] => (
    init: PropertyList[Init],
    k: SingletonType[K],
  ) => PropertyList[Init || K :: V] =
    ObjectMotif.snocReq(init, k, ())

  @targetName("propListSnocOpt")
  given [Init, K <: String, V] => (
    init: PropertyList[Init],
    k: SingletonType[K],
  ) => PropertyList[Init || K :? V] =
    ObjectMotif.snocOpt(init, k, ())
}
