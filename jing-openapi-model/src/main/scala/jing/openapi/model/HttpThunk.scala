package jing.openapi.model

sealed trait HttpThunk[O] {
  type InputType

  def path: String
  def input: RequestInput[InputType]

  def runAgainst(apiBaseUrl: String)(using client: Client): client.Result[O] =
    client.runRequest(apiBaseUrl, this)
}

object HttpThunk {
  case class Impl[I, O](
    path: String,
    input: RequestInput[I],
    responseSchema: ResponseSchema[O],
  ) extends HttpThunk[O] {
    override type InputType = I
  }

  def apply[I, O](
    path: String,
    method: HttpMethod,
    input: RequestInput[I],
    responseSchema: ResponseSchema[O],
  ): HttpThunk[O] =
    Impl(path, input, responseSchema)
}
