package jing.openapi.model

case class HttpEndpoint[Is, O](
  method: HttpMethod,
  requestSchema: RequestSchema[Is],
  responseSchema: ResponseSchema[O],
) {
  /** Transform this endpoint description to an interpretation `F[Is, O]`.
   *
   * A common interpretation would be a _client endpoint_ or a _server endpoint_
   * (even though in the latter case an interpretation of the whole [[EndpointList]] might be more suitable).
   *
   * Example:
   *
   * ```
   * import jing.openapi.model.client.ClientEndpoint
   *
   * val endpoint: HttpEndpoint[Is, O] =
   *   ???
   * val clientEndpoint: ClientEndpoint[Is, O] =
   *   endpoint.interpretAs[ClientEndpoint]
   * ```
   */
  def interpretAs[F[_, _]](using i: HttpEndpoint.Interpreter { type Endpoint[Is, O] = F[Is, O] }): F[Is, O] =
    i.interpret(this)

  /** Interpret this endpoint description using a `given Interpreter`.
   * Like [[interpretAs]], but the return type is determined by the `Interpreter` instance.
   */
  def interpret(using i: HttpEndpoint.Interpreter): i.Endpoint[Is, O] =
    i.interpret(this)
}

object HttpEndpoint {
  trait Interpreter {
    type Endpoint[I, O]

    def interpret[I, O](ep: HttpEndpoint[I, O]): Endpoint[I, O]
  }
}
