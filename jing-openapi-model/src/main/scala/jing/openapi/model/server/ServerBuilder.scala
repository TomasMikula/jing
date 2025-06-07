package jing.openapi.model.server

import jing.macroUtil.TupledFunctions
import jing.openapi.model.{EndpointList, HttpEndpoint}
import jing.openapi.model.NamedTuples.Uncons
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq
import scala.NamedTuple.{AnyNamedTuple, DropNames, NamedTuple, Names}
import scala.annotation.experimental

trait ServerBuilder extends EndpointList.Interpreter {
  import ServerBuilder.*

  type RequestHandler[I, O]
  type ServerDefinition

  def build(endpointHandlers: List[EndpointHandler[RequestHandler]]): ServerDefinition

  override type Result[Endpoints <: AnyNamedTuple] =
    PendingHandlers[Names[Endpoints], DropNames[Endpoints], RequestHandler, ServerDefinition]

  override def interpret[Endpoints <: AnyNamedTuple](endpoints: EndpointList[Endpoints]): Result[Endpoints] =
    PendingHandlers(
      endpoints.ntExpand,
      build(_),
    )
}

object ServerBuilder {
  /** A pair of `HttpEndpoint[I, O]` and its corresponding `RequestHandler[I, O]`, for some types `I`, `O`. */
  sealed trait EndpointHandler[RequestHandler[_, _]] {
    type Inputs
    type Output

    val endpoint: HttpEndpoint[Inputs, Output]
    val requestHandler: RequestHandler[Inputs, Output]
  }

  object EndpointHandler {
    def apply[A, B, F[_, _]](ep: HttpEndpoint[A, B], h: F[A, B]): EndpointHandler[F] =
      new EndpointHandler[F] {
        override type Inputs = A
        override type Output = B

        override val endpoint: HttpEndpoint[A, B] = ep
        override val requestHandler: F[A, B] = h
      }
  }

  class PendingHandlers[
    EndpointNames <: Tuple,
    EndpointTypes <: Tuple,
    RequestHandler[_, _],
    ServerDefinition,
  ](
    endpoints: EndpointList[NamedTuple[EndpointNames, EndpointTypes]],
    build: (handlers: List[EndpointHandler[RequestHandler]]) => ServerDefinition,
  ) {
    type MatchingHandler[Endpoint] = Endpoint match
      case HttpEndpoint[a, b] => RequestHandler[a, b]

    type HandlerTypes = Tuple.Map[EndpointTypes, MatchingHandler]

    private def buildServer(handlers: NamedTuple[EndpointNames, HandlerTypes]): ServerDefinition =
      build(
        endpoints.zipWithMapped[MatchingHandler, EndpointHandler[RequestHandler]](handlers) {
          [A, B] => (ep: HttpEndpoint[A, B], h: RequestHandler[A, B]) => EndpointHandler(ep, h)
        }
      )

    /** Accepts request handlers as named tuple.
     *
     * Most straightforward on the implementation side,
     * but not getting IDE hints on individual named tuple members (yet?).
     */
    def withRequestHandlersTuple(handlers: NamedTuple[EndpointNames, HandlerTypes]): ServerDefinition =
      buildServer(handlers)

    /** A function that accepts request handlers as individual parameters.
     *
     * **Caveats:**
     *
     * - Not yet working for more than 22 endpoints, due to https://github.com/scala/scala3/issues/23313.
     *
     * - Not getting IDE hints for individual parameters, due to https://github.com/scalameta/metals/issues/7532.
     *
     * - Relies on internal compiler APIs to synthesize the function type of arbitrary arity.
     *   (https://github.com/scala/scala3/discussions/23326)
     */
    transparent inline def withRequestHandlers =
      TupledFunctions.untupled(buildServer)

    /** A structurally typed object with an apply method accepting request handlers.
     *
     * **Caveats:**
     *
     * - Not yet working for more than 22 endpoints, due to https://github.com/scala/scala3/issues/23313.
     *
     * - Not getting IDE hints for individual parameters, due to https://github.com/scalameta/metals/issues/7537.
     */
    @experimental("Has experimental dependencies")
    transparent inline def implementRequestHandlers =
      TupledFunctions.untupledMethod[EndpointNames, HandlerTypes, ServerDefinition](buildServer)

    def handle(using
      u: Uncons[NamedTuple[EndpointNames, EndpointTypes]],
    ): HandlerAccumulator.PendingNext[u.HeadName, u.HeadType, u.TailNames, u.TailTypes] =
      HandlerAccumulator.PendingNext(Nil, TypeEq(u.evidence).substUpperBounded(endpoints))

    class HandlerAccumulator[Remaining <: AnyNamedTuple](
      acc: List[EndpointHandler[RequestHandler]],
      remaining: EndpointList[Remaining],
    ) {
      def handle(using u: Uncons[Remaining]): HandlerAccumulator.PendingNext[u.HeadName, u.HeadType, u.TailNames, u.TailTypes] =
        HandlerAccumulator.PendingNext(acc, TypeEq(u.evidence).substUpperBounded(remaining))

      def end(using Remaining =:= NamedTuple.Empty): ServerDefinition =
        build(acc.reverse)
    }

    object HandlerAccumulator {
      class PendingNext[HeadName, HeadType, TailNames <: Tuple, TailTypes <: Tuple](
        acc: List[EndpointHandler[RequestHandler]],
        remaining: EndpointList[NamedTuple[HeadName *: TailNames, HeadType *: TailTypes]],
      ) {
        def apply[EndpointName](using EndpointName =:= HeadName)(
          handler: MatchingHandler[HeadType]
        ): HandlerAccumulator[NamedTuple[TailNames, TailTypes]] =
          val (e, es) = remaining.uncons
          val h: EndpointHandler[RequestHandler] =
            e match
              case Indeed(Indeed((e, ev))) =>
                EndpointHandler(e, ev.substituteCo[MatchingHandler](handler))

          HandlerAccumulator(h :: acc, es)
      }
    }
  }
}
