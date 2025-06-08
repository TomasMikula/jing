package jing.openapi.model.server

import jing.macroUtil.TupledFunctions
import jing.openapi.model.{::, ||, EndpointList, HttpEndpoint}
import scala.NamedTuple.{AnyNamedTuple, DropNames, NamedTuple, Names}
import scala.annotation.experimental

trait ServerBuilder extends EndpointList.Interpreter {
  import ServerBuilder.*

  type RequestHandler[I, O]
  type ServerDefinition

  def build(endpointHandlers: List[EndpointHandler[RequestHandler]]): ServerDefinition

  override type Result[Endpoints, EndpointsTuple <: AnyNamedTuple] =
    PendingHandlers[RequestHandler, ServerDefinition, Endpoints, Names[EndpointsTuple], DropNames[EndpointsTuple]]

  override def interpret[Endpoints, EndpointsTuple <: AnyNamedTuple](
    endpoints: EndpointList[Endpoints, EndpointsTuple],
  ): Result[Endpoints, EndpointsTuple] =
    PendingHandlers(
      build(_),
      endpoints.ntExpand,
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
    RequestHandler[_, _],
    ServerDefinition,
    Endpoints,
    EndpointNames <: Tuple,
    EndpointTypes <: Tuple,
  ](
    private[PendingHandlers] val build: (handlers: List[EndpointHandler[RequestHandler]]) => ServerDefinition,
    private[PendingHandlers] val endpoints: EndpointList[Endpoints, NamedTuple[EndpointNames, EndpointTypes]],
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
  }

  object PendingHandlers {
    extension [ReqHandler[_, _], ServerDefn, Name, In, Out, Tail](
      ph: PendingHandlers[ReqHandler, ServerDefn, Name :: HttpEndpoint[In, Out] || Tail, ?, ?]
    ) {
      /** Handle the next endpoint.
       *
       * Request handler is passed to the apply method of the resulting object.
       * This indirection provides better IDE hints than [[on]], because it works around
       * https://github.com/scalameta/metals/issues/7556.
       */
      def handle: HandlerAccumulator.PendingNext[ReqHandler, ServerDefn, Name, In, Out, Tail] =
        HandlerAccumulator.PendingNext(ph.build, Nil, ph.endpoints)

      /** Handle the first endpoint using the given request handler. */
      def on[EndpointName](using EndpointName =:= Name)(
          handler: ReqHandler[In, Out],
        ): HandlerAccumulator[ReqHandler, ServerDefn, Tail] =
          val (e, es) = ph.endpoints.uncons
          val h: EndpointHandler[ReqHandler] =
            EndpointHandler(e, handler)
          HandlerAccumulator(ph.build, h :: Nil, es)
    }

    class HandlerAccumulator[ReqHandler[_, _], ServerDefn, Remaining](
      private[HandlerAccumulator] val build: (handlers: List[EndpointHandler[ReqHandler]]) => ServerDefn,
      private[HandlerAccumulator] val acc: List[EndpointHandler[ReqHandler]],
      private[HandlerAccumulator] val remaining: EndpointList[Remaining, ?],
    ) {
      def end(using Remaining =:= Void): ServerDefn =
        build(acc.reverse)
    }

    object HandlerAccumulator {
      // might seem superfluous, but helps reduce the types in the IDE by
      // working around https://github.com/scalameta/metals/issues/7556
      class PendingNext[ReqHandler[_, _], ServerDefn, Name, In, Out, Tail](
        build: (handlers: List[EndpointHandler[ReqHandler]]) => ServerDefn,
        acc: List[EndpointHandler[ReqHandler]],
        remaining: EndpointList[Name :: HttpEndpoint[In, Out] || Tail, ?],
      ) {
        def apply[EndpointName](using EndpointName =:= Name)(
          handler: ReqHandler[In, Out],
        ): HandlerAccumulator[ReqHandler, ServerDefn, Tail] =
          val (e, es) = remaining.uncons
          val h: EndpointHandler[ReqHandler] =
            EndpointHandler(e, handler)

          HandlerAccumulator(build, h :: acc, es)
      }

      extension [ReqHandler[_, _], ServerDefn, Name, In, Out, Tail](
        acc: HandlerAccumulator[ReqHandler, ServerDefn, Name :: HttpEndpoint[In, Out] || Tail]
      ) {

        /** Handle the next endpoint.
         *
         * Request handler is passed to the apply method of the resulting object.
         * This indirection provides better IDE hints than [[on]], because it works around
         * https://github.com/scalameta/metals/issues/7556.
         */
        def handle: HandlerAccumulator.PendingNext[ReqHandler, ServerDefn, Name, In, Out, Tail] =
          HandlerAccumulator.PendingNext(acc.build, acc.acc, acc.remaining)

        /** Handle the next endpoint using the given request handler. */
        def on[EndpointName](using EndpointName =:= Name)(
            handler: ReqHandler[In, Out],
          ): HandlerAccumulator[ReqHandler, ServerDefn, Tail] =
            val (e, es) = acc.remaining.uncons
            val h: EndpointHandler[ReqHandler] =
              EndpointHandler(e, handler)
            HandlerAccumulator(acc.build, h :: acc.acc, es)
      }
    }
  }
}
