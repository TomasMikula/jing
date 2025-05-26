package jing.openapi.model

import libretto.lambda.util.{SingletonType, TypeEq}
import libretto.lambda.util.TypeEq.Refl
import scala.NamedTuple.{AnyNamedTuple, NamedTuple}
import scala.NamedTupleDecomposition.DropNames

sealed trait EndpointList[EPs <: AnyNamedTuple] {
  type Names <: Tuple
  type Types <: Tuple
  type Endpoints = EPs

  def evidence: EPs =:= NamedTuple[Names, Types]

  def tuple: Types

  def namedTuple: EPs =
    evidence.flip(NamedTuple[Names, Types](tuple))

  def ::[A, B](name: String, ep: HttpEndpoint[A, B]): EndpointList[NamedTuples.Cons[name.type, HttpEndpoint[A, B], EPs]] =
    EndpointList.Cons(SingletonType(name), ep, this)

  def interpret(using i: EndpointList.Interpreter): i.Result[EPs] =
    i.interpret(this)

  def zipWithMapped[F[_], C](that: NamedTuple[NamedTuple.Names[EPs], Tuple.Map[NamedTuple.DropNames[EPs], F]])(
    f: [A, B] => (HttpEndpoint[A, B], F[HttpEndpoint[A, B]]) => C,
  ): List[C]
}

object EndpointList {
  case object Empty extends EndpointList[NamedTuple.Empty] {
    override type Names = EmptyTuple
    override type Types = EmptyTuple

    override def evidence: NamedTuple.Empty =:= NamedTuple[EmptyTuple, EmptyTuple] =
      summon

    override def tuple: Types =
      EmptyTuple

    override def zipWithMapped[F[_], C](
      that: NamedTuple[EmptyTuple, EmptyTuple]
    )(
      f: [A, B] => (e: HttpEndpoint[A, B], fe: F[HttpEndpoint[A, B]]) => C,
    ): List[C] =
      Nil
  }

  case class Cons[S <: String, A, B, T <: AnyNamedTuple](
    headName: SingletonType[S],
    headValue: HttpEndpoint[A, B],
    tail: EndpointList[T],
  ) extends EndpointList[NamedTuples.Cons[S, HttpEndpoint[A, B], T]] {
    override type Names = S *: tail.Names
    override type Types = HttpEndpoint[A, B] *: tail.Types

    override def evidence: =:=[
      NamedTuples.Cons[S, HttpEndpoint[A, B], T],
      NamedTuples.Cons[S, HttpEndpoint[A, B], NamedTuple[tail.Names, tail.Types]]
    ] =
      TypeEq(tail.evidence)
        .liftUpperBounded[AnyNamedTuple, [t <: AnyNamedTuple] =>> NamedTuples.Cons[S, HttpEndpoint[A, B], t]]
        .to_=:=

    override def tuple: Types = headValue *: tail.tuple

    override def zipWithMapped[F[_], C](
      that: NamedTuple[S *: NamedTuple.Names[T], Tuple.Map[HttpEndpoint[A, B] *: DropNames[T], F]],
    )(
      f: [A, B] => (e: HttpEndpoint[A, B], fe: F[HttpEndpoint[A, B]]) => C,
    ): List[C] =
      f(headValue, that.head) :: tail.zipWithMapped[F, C](that.tail)(f)
  }

  trait Interpreter {
    type Result[Endpoints <: AnyNamedTuple]

    def interpret[Endpoints <: AnyNamedTuple](eps: EndpointList[Endpoints]): Result[Endpoints]
  }

  def empty: EndpointList[NamedTuple.Empty] = Empty
}
