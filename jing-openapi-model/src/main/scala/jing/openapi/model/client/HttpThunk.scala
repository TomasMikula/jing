package jing.openapi.model.client

import jing.openapi.model.*

sealed trait HttpThunk[+MimeType, O] {
  type Params
  type BodyType

  def method: HttpMethod

  def paramsSchema: RequestSchema.Params[Params]

  /** includes path and query */
  def params: Value[Obj[Params]]

  def body: Option[(
    schema: BodySchema.NonEmpty[BodyType],
    body: Body[MimeType, BodyType],
  )]

  def runAgainst(apiBaseUrl: String)(using
    client: Client,
    witness: MimeType <:< client.SupportedMimeType,
  ): client.Response[O] =
    client.runRequest(apiBaseUrl, witness.substituteCo[HttpThunk[_, O]](this))
}

object HttpThunk {
  case class Impl[Ps, MimeType, Bdy, O](
    method: HttpMethod,
    paramsSchema: RequestSchema.Params[Ps],
    params: Value[Obj[Ps]],
    body: Option[(
      schema: BodySchema.NonEmpty[Bdy],
      body: Body[MimeType, Bdy],
    )],
    responseSchema: ResponseSchema[O],
  ) extends HttpThunk[MimeType, O] {
    override type Params = Ps
    override type BodyType = Bdy
  }

  def apply[Ps, MimeType, Bdy, O](
    method: HttpMethod,
    paramsSchema: RequestSchema.Params[Ps],
    params: Value[Obj[Ps]],
    body: Option[(
      schema: BodySchema.NonEmpty[Bdy],
      body: Body[MimeType, Bdy],
    )],
    responseSchema: ResponseSchema[O],
  ): HttpThunk[MimeType, O] =
    Impl(method, paramsSchema, params, body, responseSchema)
}
