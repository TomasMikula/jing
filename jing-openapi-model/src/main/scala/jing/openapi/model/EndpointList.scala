package jing.openapi.model

import libretto.lambda.util.SingletonType
import scala.NamedTuple.{AnyNamedTuple, NamedTuple}

sealed trait EndpointList[EPs <: AnyNamedTuple] {
  type Endpoints = EPs

  def ::[A, B](name: String, ep: HttpEndpoint[A, B]): EndpointList[NamedTuple.Concat[NamedTuple[name.type *: EmptyTuple, HttpEndpoint[A, B] *: EmptyTuple], EPs]] =
    EndpointList.Cons(SingletonType(name), ep, this)
}

object EndpointList {
  case object Empty extends EndpointList[NamedTuple.Empty]

  case class Cons[S <: String, A, B, T <: AnyNamedTuple](
    headName: SingletonType[S],
    headValue: HttpEndpoint[A, B],
    tail: EndpointList[T],
  ) extends EndpointList[NamedTuple.Concat[NamedTuple[S *: EmptyTuple, HttpEndpoint[A, B] *: EmptyTuple], T]]

  def empty: EndpointList[NamedTuple.Empty] = Empty
}
