package jing.openapi.model

import libretto.lambda.util.{Exists, SingletonType, TypeEq}
import libretto.lambda.util.TypeEq.Refl
import scala.NamedTuple.{AnyNamedTuple, DropNames, NamedTuple, Names}
import jing.openapi.model.NamedTuples.Uncons

sealed trait EndpointList[EPs <: AnyNamedTuple] {
  type Endpoints = EPs

  def evidence: EPs =:= NamedTuple[Names[EPs], DropNames[EPs]]

  def tuple: DropNames[EPs]

  def uncons(using u: NamedTuples.Uncons[EPs]): (
    Exists[[A] =>> Exists[[B] =>> (HttpEndpoint[A, B], u.HeadType =:= HttpEndpoint[A, B])]],
    EndpointList[u.Tail]
  )

  def namedTuple: EPs =
    evidence.flip((tuple))

  def ntExpand: EndpointList[NamedTuple[Names[EPs], DropNames[EPs]]] =
    TypeEq(evidence).substUpperBounded(this)

  def *:[A, B](name: String, ep: HttpEndpoint[A, B]): EndpointList[NamedTuples.Cons[name.type, HttpEndpoint[A, B], EPs]] =
    EndpointList.Cons(SingletonType(name), ep, this)

  def interpret(using i: EndpointList.Interpreter): i.Result[EPs] =
    i.interpret(this)

  def zipWithMapped[F[_], C](that: NamedTuple[NamedTuple.Names[EPs], Tuple.Map[NamedTuple.DropNames[EPs], F]])(
    f: [A, B] => (HttpEndpoint[A, B], F[HttpEndpoint[A, B]]) => C,
  ): List[C]
}

object EndpointList {
  case object Empty extends EndpointList[NamedTuple.Empty] {
    override def evidence: NamedTuple.Empty =:= NamedTuple[EmptyTuple, EmptyTuple] =
      summon

    override def tuple: EmptyTuple =
      EmptyTuple

    override def uncons(using u: Uncons[scala.NamedTuple.Empty]): (
      Exists[[A] =>> Exists[[B] =>> (HttpEndpoint[A, B], u.HeadType =:= HttpEndpoint[A, B])]],
      EndpointList[u.Tail],
    ) = {
      // absurd
      u.evidence: =:=[
        NamedTuple[       EmptyTuple        ,        EmptyTuple        ],
        NamedTuple[u.HeadName *: u.TailNames, u.HeadType *: u.TailTypes]
      ]
      throw AssertionError("Impossible: empty tuple is non-empty")
    }

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
    override def evidence: =:=[
      NamedTuples.Cons[S, HttpEndpoint[A, B], T],
      NamedTuples.Cons[S, HttpEndpoint[A, B], NamedTuple[Names[T], DropNames[T]]]
    ] =
      TypeEq(tail.evidence)
        .liftUpperBounded[AnyNamedTuple, [t <: AnyNamedTuple] =>> NamedTuples.Cons[S, HttpEndpoint[A, B], t]]
        .to_=:=

    override def tuple: HttpEndpoint[A, B] *: DropNames[T] =
      headValue *: tail.tuple

    override def uncons(using u: Uncons[NamedTuples.Cons[S, HttpEndpoint[A, B], T]]): (
      Exists[[A] =>> Exists[[B] =>> (HttpEndpoint[A, B], u.HeadType =:= HttpEndpoint[A, B])]],
      EndpointList[u.Tail],
    ) =
      // Named tuples are a misfit for this:
      //  - cannot prove injectivity;
      //  - bounds (e.g. N <: Tuple) only complicate things (e.g. cannot fabricadte BiInjectivity instance, because of the bounds) at no benefit.
      // Should back out of named tuples and avoid these unsafe casts.
      ( Exists(Exists((headValue, summon[u.HeadType =:= u.HeadType].asInstanceOf[u.HeadType =:= HttpEndpoint[A, B]])))
      , tail.asInstanceOf[EndpointList[u.Tail]]
      )

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
