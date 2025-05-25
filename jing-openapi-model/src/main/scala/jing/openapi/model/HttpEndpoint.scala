package jing.openapi.model

case class HttpEndpoint[Is, O](
  method: HttpMethod,
  requestSchema: RequestSchema[Is],
  responseSchema: ResponseSchema[O],
) {
  def interpret(using impl: HttpEndpoint.Interpreter): impl.Endpoint[Is, O] =
    impl.interpret(this)

  def as[F[_, _]](using HttpEndpoint.Interpreter { type Endpoint[Is, O] = F[Is, O] }): F[Is, O] =
    interpret
}

object HttpEndpoint {
  trait Interpreter {
    type Endpoint[I, O]

    def interpret[I, O](ep: HttpEndpoint[I, O]): Endpoint[I, O]
  }
}
