package jing.openapi.model.client

import jing.openapi.model.*

sealed trait HttpThunk[O] {
  type InputType

  def path: String
  def method: HttpMethod
  def input: RequestInput[?, InputType]

  def runAgainst(apiBaseUrl: String)(using client: Client): client.Response[O] =
    client.runRequest(apiBaseUrl, this)
}

object HttpThunk {
  case class Impl[I, O](
    path: String,
    method: HttpMethod,
    input: RequestInput[?, I],
    responseSchema: ResponseSchema[O],
  ) extends HttpThunk[O] {
    override type InputType = I
  }

  def apply[I, O](
    path: String,
    method: HttpMethod,
    input: RequestInput[?, I],
    responseSchema: ResponseSchema[O],
  ): HttpThunk[O] =
    Impl(path, method, input, responseSchema)
}
