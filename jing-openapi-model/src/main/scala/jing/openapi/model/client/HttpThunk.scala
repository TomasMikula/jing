package jing.openapi.model.client

import jing.openapi.model.*

sealed trait HttpThunk[+MimeType, O] {
  type QueryParams
  type BodyType

  def path: String
  def method: HttpMethod
  def queryParams: Option[Value[Obj[QueryParams]]]
  def body: Option[Body[MimeType, BodyType]]

  def runAgainst(apiBaseUrl: String)(using
    client: Client,
    witness: MimeType <:< client.SupportedMimeType,
  ): client.Response[O] =
    client.runRequest(apiBaseUrl, witness.substituteCo[HttpThunk[_, O]](this))
}

object HttpThunk {
  case class Impl[QParams, MimeType, Bdy, O](
    path: String,
    method: HttpMethod,
    queryParams: Option[Value[Obj[QParams]]],
    body: Option[Body[MimeType, Bdy]],
    responseSchema: ResponseSchema[O],
  ) extends HttpThunk[MimeType, O] {
    override type QueryParams = QParams
    override type BodyType = Bdy
  }

  def apply[QParams, MimeType, Bdy, O](
    path: String,
    method: HttpMethod,
    queryParams: Option[Value[Obj[QParams]]],
    body: Option[Body[MimeType, Bdy]],
    responseSchema: ResponseSchema[O],
  ): HttpThunk[MimeType, O] =
    Impl(path, method, queryParams, body, responseSchema)
}
