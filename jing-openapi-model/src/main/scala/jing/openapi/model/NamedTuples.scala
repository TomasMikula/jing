package jing.openapi.model

import scala.NamedTuple.*

object NamedTuples {
  type Cons[N, T, Tail <: AnyNamedTuple] =
    NamedTuple[N *: Names[Tail], T *: DropNames[Tail]]
}
