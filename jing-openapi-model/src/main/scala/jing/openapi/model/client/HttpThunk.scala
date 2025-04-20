package jing.openapi.model.client

import jing.openapi.model.*

sealed trait HttpThunk[+MimeType, O] {
  type InputType

  def path: String
  def method: HttpMethod
  def input: RequestInput[MimeType, InputType]

  def runAgainst(apiBaseUrl: String)(using
    client: Client,
    witness: MimeType <:< client.SupportedMimeType,
  ): client.Response[O] =
    client.runRequest(apiBaseUrl, witness.substituteCo[HttpThunk[_, O]](this))
}

object HttpThunk {
  case class Impl[MimeType, I, O](
    path: String,
    method: HttpMethod,
    input: RequestInput[MimeType, I],
    responseSchema: ResponseSchema[O],
  ) extends HttpThunk[MimeType, O] {
    override type InputType = I
  }

  def apply[MimeType, I, O](
    path: String,
    method: HttpMethod,
    input: RequestInput[MimeType, I],
    responseSchema: ResponseSchema[O],
  ): HttpThunk[MimeType, O] =
    Impl(path, method, input, responseSchema)
}
