package jing.openapi.model.server

import jing.macroUtil.TupledFunctions
import jing.openapi.model.{EndpointList, HttpEndpoint}
import scala.NamedTuple.{AnyNamedTuple, DropNames, NamedTuple, Names}
import scala.annotation.experimental

trait ServerBuilder extends EndpointList.Interpreter {
  import ServerBuilder.*

  type RequestHandler[I, O]
  type ServerDefinition

  def build(endpointHandlers: List[EndpointHandler[RequestHandler]]): ServerDefinition

  type MatchingHandler[Endpoint] = Endpoint match
    case HttpEndpoint[a, b] => RequestHandler[a, b]

  override type Result[Endpoints <: AnyNamedTuple] =
    PendingHandlers[Names[Endpoints], Tuple.Map[DropNames[Endpoints], MatchingHandler], ServerDefinition]

  override def interpret[Endpoints <: AnyNamedTuple](endpoints: EndpointList[Endpoints]): Result[Endpoints] =
    PendingHandlers(
      (handlers: NamedTuple.Map[Endpoints, MatchingHandler]) => build(
        endpoints.zipWithMapped[MatchingHandler, EndpointHandler[RequestHandler]](handlers) {
          [A, B] => (ep: HttpEndpoint[A, B], h: RequestHandler[A, B]) => EndpointHandler(ep, h)
        }
      )
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

  class PendingHandlers[HandlerNames <: Tuple, HandlerTypes <: Tuple, ServerDefinition](
    buildServer: NamedTuple[HandlerNames, HandlerTypes] => ServerDefinition,
  ) {
    /** Accepts request handlers as named tuple.
     *
     * Most straightforward on the implementation side,
     * but not getting IDE hints on individual named tuple members (yet?).
     */
    def withRequestHandlersTuple(handlers: NamedTuple[HandlerNames, HandlerTypes]): ServerDefinition =
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
      TupledFunctions.untupledMethod[HandlerNames, HandlerTypes, ServerDefinition](buildServer)
  }
}
