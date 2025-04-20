package jing.openapi.model.client

import jing.openapi.model.*

class ClientEndpoint[Is, O](
  private val underlying: HttpEndpoint[Is, O],
) {
  import ClientEndpoint.*

  private[ClientEndpoint] def withInput[MimeType](
    in: RequestInput[MimeType, Is],
  ): HttpThunk[MimeType, O] =
    import underlying.{path, method, responseSchema}
    HttpThunk(path, method, in, responseSchema)

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
          case RequestSchema.Body(schema) => schema
      val input: RequestInput[MimeType, Void || "body" :: DiscriminatedUnion[Bs]] =
        RequestInput.bodyOnly(bodySchema, i, body)
      endpoint.withInput(input)

  class WithQueryParams[Is, Qs, Remaining, O](
    endpoint: ClientEndpoint[Is, O],
    params: Value[Obj[Qs]],
  ) {
    // TODO: body method

    def toRequest(using ev1: (Void || "params" :: Obj[Qs]) =:= Is, ev2: Remaining =:= Void): HttpThunk[Nothing, O] =
      endpoint.withInput(ev1.substituteCo(RequestInput.Params(params)))

    def runAgainst(using (Void || "params" :: Obj[Qs]) =:= Is, Remaining =:= Void)(
      apiBaseUrl: String,
    )(using
      client: Client,
    ): client.Response[O] = {
      toRequest.runAgainst(apiBaseUrl)
    }
  }
}
