package jing.openapi.model

trait EndpointInterpreter {
  type Endpoint[I, O]

  def interpret[I, O](ep: HttpEndpoint[I, O]): Endpoint[I, O]
}
