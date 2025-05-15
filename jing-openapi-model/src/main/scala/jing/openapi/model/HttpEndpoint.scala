package jing.openapi.model

case class HttpEndpoint[Is, O](
  method: HttpMethod,
  requestSchema: RequestSchema[Is],
  responseSchema: ResponseSchema[O],
) {
  def interpret(using impl: EndpointInterpreter): impl.Endpoint[Is, O] =
    impl.interpret(this)

  def as[F[_, _]](using EndpointInterpreter { type Endpoint[Is, O] = F[Is, O] }): F[Is, O] =
    interpret
}
