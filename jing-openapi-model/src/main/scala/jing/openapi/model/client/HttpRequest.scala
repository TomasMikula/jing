package jing.openapi.model.client

import jing.openapi.model.*
import scala.annotation.implicitNotFound

sealed trait HttpRequest[+MimeType, O] {
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
    @implicitNotFound(
      "No given Client instance in scope to run this request.\n" +
      "To use the default Java HTTP client, import jing.openapi.client.default.instance " +
      "(you might need to add \"dev.continuously.jing\" %% \"jing-openapi-client-default\" % \"<version>\" to library dependencies).\n" +
      "Other Client instances might be available via integrations.\n"
    )
    client: Client,
    witness: MimeType <:< client.SupportedMimeType,
  ): client.Response[O] =
    client.runRequest(apiBaseUrl, witness.substituteCo[HttpRequest[_, O]](this))
}

object HttpRequest {
  case class Impl[Ps, MimeType, Bdy, O](
    method: HttpMethod,
    paramsSchema: RequestSchema.Params[Ps],
    params: Value[Obj[Ps]],
    body: Option[(
      schema: BodySchema.NonEmpty[Bdy],
      body: Body[MimeType, Bdy],
    )],
    responseSchema: ResponseSchema[O],
  ) extends HttpRequest[MimeType, O] {
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
  ): HttpRequest[MimeType, O] =
    Impl(method, paramsSchema, params, body, responseSchema)
}
