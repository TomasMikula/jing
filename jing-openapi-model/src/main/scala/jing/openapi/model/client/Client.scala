package jing.openapi.model.client

import jing.openapi.model.*

trait Client extends EndpointInterpreter {
  type Response[T]

  def runRequest[O](
    baseUrl: String,
    req: HttpThunk[O],
  ): Response[O]

  override type Endpoint[I, O] = ClientEndpoint[I, O]

  override def interpret[I, O](ep: HttpEndpoint[I, O]): ClientEndpoint[I, O] =
    ClientEndpoint(ep)
}
