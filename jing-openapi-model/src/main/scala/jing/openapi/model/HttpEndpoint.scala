package jing.openapi.model

case class HttpEndpoint[Is, O](
  method: HttpMethod,
  requestSchema: RequestSchema[Is],
  responseSchema: ResponseSchema[O],
) {
  def interpret(using impl: EndpointInterpreter): impl.Endpoint[Is, O] =
    impl.interpret(this)
}
