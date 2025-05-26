package jing.openapi.model.server

import jing.openapi.model.{EndpointList, HttpEndpoint}
import scala.NamedTuple.AnyNamedTuple

trait ServerBuilder extends EndpointList.Interpreter {
  import ServerBuilder.*

  type RequestHandler[I, O]
  type Server

  def build(endpointHandlers: List[EndpointHandler[RequestHandler]]): Server

  type MatchingHandler[Endpoint] = Endpoint match
    case HttpEndpoint[a, b] => RequestHandler[a, b]

  override type Result[Endpoints <: AnyNamedTuple] =
    NamedTuple.Map[Endpoints, MatchingHandler] => Server

  override def interpret[Endpoints <: AnyNamedTuple](endpoints: EndpointList[Endpoints]): Result[Endpoints] =
    (handlers) => build(
      endpoints.zipWithMapped[MatchingHandler, EndpointHandler[RequestHandler]](handlers) {
        [A, B] => (ep: HttpEndpoint[A, B], h: RequestHandler[A, B]) => EndpointHandler(ep, h)
      }
    )
}

object ServerBuilder {
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
}
