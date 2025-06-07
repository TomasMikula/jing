package jing.openapi.model

import libretto.lambda.util.BiInjective
import scala.NamedTuple.*

object NamedTuples {
  type Cons[N, T, Tail <: AnyNamedTuple] =
    NamedTuple[N *: Names[Tail], T *: DropNames[Tail]]

  /** Transpose of a named tuple type.
   *
   *     Transpose[NamedTuple[N1 *: N2 *: ..., T1 *: T2 *: ...]] = (N1, T1) *: (N2, T2) *: ...
   */
  type Transpose[NT <: AnyNamedTuple] =
    NT match
      case NamedTuple.Empty => EmptyTuple
      case NamedTuple[n *: ns, t *: ts] => (n, t) *: Transpose[NamedTuple[ns, ts]]

  sealed trait Uncons[NT] {
    type HeadName
    type HeadType
    type TailNames <: Tuple
    type TailTypes <: Tuple
    type Tail <: AnyNamedTuple

    def evidence: NT =:= NamedTuple[HeadName *: TailNames, HeadType *: TailTypes]
  }

  object Uncons {
    given uncons[N0, T0, Ns <: Tuple, Ts <: Tuple]: (Uncons[NamedTuple[N0 *: Ns, T0 *: Ts]] {
      type HeadName = N0
      type HeadType = T0
      type TailNames = Ns
      type TailTypes = Ts
      type Tail = NamedTuple[Ns, Ts]
    }) = new Uncons[NamedTuple[N0 *: Ns, T0 *: Ts]] {
      override type HeadName = N0
      override type HeadType = T0
      override type TailNames = Ns
      override type TailTypes = Ts
      override type Tail = NamedTuple[Ns, Ts]

      override def evidence: NamedTuple[N0 *: Ns, T0 *: Ts] =:= NamedTuple[N0 *: Ns, T0 *: Ts] =
        summon
    }
  }
}
