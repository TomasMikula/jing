package jing.openapi.model

case class HttpEndpoint[I, O](
  path: String,
  method: HttpMethod,
  requestSchema: RequestSchema[I],
  responseSchema: ResponseSchema[O],
) {
  def interpret(using impl: EndpointInterpreter): impl.Endpoint[I, O] =
    impl.interpret(this)
}
