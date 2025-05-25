package jing.openapi.model

import libretto.lambda.util.{SingletonType, TypeEq}
import libretto.lambda.util.TypeEq.Refl
import scala.NamedTuple.{AnyNamedTuple, NamedTuple}

sealed trait EndpointList[EPs <: AnyNamedTuple] {
  type Names <: Tuple
  type Types <: Tuple
  type Endpoints = EPs

  def evidence: EPs =:= NamedTuple[Names, Types]

  def tuple: Types

  def namedTuple: EPs =
    evidence.flip(NamedTuple[Names, Types](tuple))

  def ::[A, B](name: String, ep: HttpEndpoint[A, B]): EndpointList[NamedTuple.Concat[NamedTuple[name.type *: EmptyTuple, HttpEndpoint[A, B] *: EmptyTuple], EPs]] =
    EndpointList.Cons(SingletonType(name), ep, this)
}

object EndpointList {
  case object Empty extends EndpointList[NamedTuple.Empty] {
    override type Names = EmptyTuple
    override type Types = EmptyTuple

    override def evidence: NamedTuple.Empty =:= NamedTuple[EmptyTuple, EmptyTuple] =
      summon

    override def tuple: Types =
      EmptyTuple
  }

  case class Cons[S <: String, A, B, T <: AnyNamedTuple](
    headName: SingletonType[S],
    headValue: HttpEndpoint[A, B],
    tail: EndpointList[T],
  // ) extends EndpointList[NamedTuple.Concat[NamedTuple[S *: EmptyTuple, HttpEndpoint[A, B] *: EmptyTuple], T]] {
  ) extends EndpointList[NamedTuple[S *: NamedTuple.Names[T], HttpEndpoint[A, B] *: NamedTuple.DropNames[T]]] {
    override type Names = S *: tail.Names
    override type Types = HttpEndpoint[A, B] *: tail.Types

    override def evidence: =:=[
      NamedTuple[S *: NamedTuple.Names[T], HttpEndpoint[A, B] *: NamedTuple.DropNames[T]],
      NamedTuple[S *: tail.Names         , HttpEndpoint[A, B] *: tail.Types]
    ] = {
      val ev: TypeEq[T, NamedTuple[tail.Names, tail.Types]] =
        TypeEq(tail.evidence)
      val ev1: TypeEq[
        NamedTuple[S *: NamedTuple.Names[T], HttpEndpoint[A, B] *: NamedTuple.DropNames[T]],
        NamedTuple[S *: tail.Names         , HttpEndpoint[A, B] *: NamedTuple.DropNames[T]]
      ] =
        ev.liftUpperBounded[AnyNamedTuple, NamedTuple.Names]
          .liftUpperBounded[Tuple, [t <: Tuple] =>> S *: t]
          .liftUpperBounded[Tuple, [names <: Tuple] =>> NamedTuple[names, HttpEndpoint[A, B] *: NamedTuple.DropNames[T]]]
      val ev2: TypeEq[
        NamedTuple[S *: tail.Names, HttpEndpoint[A, B] *: NamedTuple.DropNames[T]],
        NamedTuple[S *: tail.Names, HttpEndpoint[A, B] *: tail.Types]
      ] =
        ev.liftUpperBounded[AnyNamedTuple, NamedTuple.DropNames]
          .liftUpperBounded[Tuple, [t <: Tuple] =>> HttpEndpoint[A, B] *: t]
          .liftUpperBounded[Tuple, [types <: Tuple] =>> NamedTuple[S *: tail.Names, types]]
      ev1.to_=:= andThen ev2.to_=:=
    }

    override def tuple: Types = headValue *: tail.tuple
  }

  def empty: EndpointList[NamedTuple.Empty] = Empty
}
