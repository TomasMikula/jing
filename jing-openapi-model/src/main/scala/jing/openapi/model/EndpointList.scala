package jing.openapi.model

import jing.openapi.model.{::, ||}
import libretto.lambda.util.TypeEq.Refl
import libretto.lambda.util.{BiInjective, Exists, SingletonType, TypeEq}

import scala.NamedTuple.{AnyNamedTuple, DropNames, NamedTuple, Names}

sealed trait EndpointList[EPs, EPTuple <: AnyNamedTuple] {
  type Endpoints = EPs
  type EndpointsTuple = EPTuple

  def evidence: EPTuple =:= NamedTuple[Names[EPTuple], DropNames[EPTuple]]

  def tuple: DropNames[EPTuple]

  def uncons[N, I, O, T](using EPs =:= (N :: HttpEndpoint[I, O] || T)): (HttpEndpoint[I, O], EndpointList[T, ?])

  def namedTuple: EPTuple =
    evidence.flip((tuple))

  def ntExpand: EndpointList[EPs, NamedTuple[Names[EPTuple], DropNames[EPTuple]]] =
    TypeEq(evidence).substUpperBounded(this)

  def *:[A, B](
    name: String,
    ep: HttpEndpoint[A, B],
  ): EndpointList[name.type :: HttpEndpoint[A, B] || EPs, NamedTuples.Cons[name.type, HttpEndpoint[A, B], EPTuple]] =
    EndpointList.Cons(SingletonType(name), ep, this)

  def interpret(using i: EndpointList.Interpreter): i.Result[EPs, EPTuple] =
    i.interpret(this)

  def zipWithMapped[F[_], C](that: NamedTuple[NamedTuple.Names[EPTuple], Tuple.Map[NamedTuple.DropNames[EPTuple], F]])(
    f: [A, B] => (HttpEndpoint[A, B], F[HttpEndpoint[A, B]]) => C,
  ): List[C]
}

object EndpointList {
  case object Empty extends EndpointList[Void, NamedTuple.Empty] {
    override def evidence: NamedTuple.Empty =:= NamedTuple[EmptyTuple, EmptyTuple] =
      summon

    override def tuple: EmptyTuple =
      EmptyTuple

    override def uncons[N, I, O, T](using ev: Void =:= (N :: HttpEndpoint[I, O] || T)): (HttpEndpoint[I, O], EndpointList[T, ?]) =
      ||.isNotVoid(using ev.flip)

    override def zipWithMapped[F[_], C](
      that: NamedTuple[EmptyTuple, EmptyTuple]
    )(
      f: [A, B] => (e: HttpEndpoint[A, B], fe: F[HttpEndpoint[A, B]]) => C,
    ): List[C] =
      Nil
  }

  case class Cons[S <: String, A, B, Es, T <: AnyNamedTuple](
    headName: SingletonType[S],
    headValue: HttpEndpoint[A, B],
    tail: EndpointList[Es, T],
  ) extends EndpointList[S :: HttpEndpoint[A, B] || Es, NamedTuples.Cons[S, HttpEndpoint[A, B], T]] {
    override def evidence: =:=[
      NamedTuples.Cons[S, HttpEndpoint[A, B], T],
      NamedTuples.Cons[S, HttpEndpoint[A, B], NamedTuple[Names[T], DropNames[T]]]
    ] =
      TypeEq(tail.evidence)
        .liftUpperBounded[AnyNamedTuple, [t <: AnyNamedTuple] =>> NamedTuples.Cons[S, HttpEndpoint[A, B], t]]
        .to_=:=

    override def tuple: HttpEndpoint[A, B] *: DropNames[T] =
      headValue *: tail.tuple

    override def uncons[N, I, O, Tail](using
      ev: (S :: HttpEndpoint[A, B] || Es) =:= (N :: HttpEndpoint[I, O] || Tail)
    ): (HttpEndpoint[I, O], EndpointList[Tail, ?]) =
      ev match
        case BiInjective[||](BiInjective[::](_, ev1), ev2) =>
          (ev1(headValue), ev2.substituteCo[EndpointList[_, T]](tail))

    override def zipWithMapped[F[_], C](
      that: NamedTuple[S *: NamedTuple.Names[T], Tuple.Map[HttpEndpoint[A, B] *: DropNames[T], F]],
    )(
      f: [A, B] => (e: HttpEndpoint[A, B], fe: F[HttpEndpoint[A, B]]) => C,
    ): List[C] =
      f(headValue, that.head) :: tail.zipWithMapped[F, C](that.tail)(f)
  }

  trait Interpreter {
    type Result[Endpoints, EndpointsTuple <: AnyNamedTuple]

    def interpret[Endpoints, EndpointsTuple <: AnyNamedTuple](
      eps: EndpointList[Endpoints, EndpointsTuple],
    ): Result[Endpoints, EndpointsTuple]
  }

  def empty: EndpointList[Void, NamedTuple.Empty] = Empty
}
