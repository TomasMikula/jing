package jing.openapi.model.client

import jing.openapi.model.*

class ClientEndpoint[Is, O](
  private val underlying: HttpEndpoint[Is, O],
) {
  import ClientEndpoint.*

  def queryParams[Qs, Rest](using ev: ToRightAssoc[Is] =:= ("params" :: Obj[Qs] || Rest))(
    params: Value[Obj[Qs]],
  ): ClientEndpoint.WithQueryParams[Is, Qs, Rest, O] =
    WithQueryParams(this, params)
}

object ClientEndpoint {
  extension [Bs, O](endpoint: ClientEndpoint[Void || "body" :: DiscriminatedUnion[Bs], O])
    def body[MimeType <: String](using i: MimeType IsCaseOf Bs)(
      body: Value[i.Type],
    ): HttpThunk[MimeType, O] =
      val bodySchema: BodySchema.NonEmpty[DiscriminatedUnion[Bs]] =
        endpoint.underlying.requestSchema match
          case RequestSchema.WithBody(RequestSchema.NoParams, schema) => schema
      import endpoint.underlying.{path, method, responseSchema}
      HttpThunk(
        path,
        method,
        queryParams = None,
        body = Some(Body(bodySchema, i, body)),
        responseSchema,
      )

  sealed trait RequestBuilder[Is, Acc, Remaining, O] {
    def toRequest(using ev1: Acc =:= Is, ev2: Remaining =:= Void): HttpThunk[Nothing, O]
  }

  class WithQueryParams[Is, Qs, Remaining, O](
    endpoint: ClientEndpoint[Is, O],
    params: Value[Obj[Qs]],
  ) extends RequestBuilder[Is, Void || "params" :: Obj[Qs], Remaining, O] {
    // TODO: body method

    override def toRequest(using
      ev1: (Void || "params" :: Obj[Qs]) =:= Is,
      ev2: Remaining =:= Void,
    ): HttpThunk[Nothing, O] =
      import endpoint.underlying.{path, method, responseSchema}
      HttpThunk(
        path,
        method,
        queryParams = Some(params),
        body = None,
        responseSchema,
      )

    def runAgainst(using (Void || "params" :: Obj[Qs]) =:= Is, Remaining =:= Void)(
      apiBaseUrl: String,
    )(using
      client: Client,
    ): client.Response[O] = {
      toRequest.runAgainst(apiBaseUrl)
    }
  }
}
