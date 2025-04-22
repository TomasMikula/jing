package jing.openapi.model.client

import jing.openapi.model.*
import jing.openapi.model.RequestSchema.ConstantPath

class ClientEndpoint[Is, O](
  private val underlying: HttpEndpoint[Is, O],
) {
  import ClientEndpoint.*

  def params[Qs, Rest](using ev: ToRightAssoc[Is] =:= ("params" :: Obj[Qs] || Rest))(
    params: Value[Obj[Qs]],
  ): ClientEndpoint.WithQueryParams[Is, Qs, Rest, O] =
    WithQueryParams(this, params)

  def params[Qs, Rest](using ev: ToRightAssoc[Is] =:= ("params" :: Obj[Qs] || Rest))(
    f: Value.ObjectBuilder[Void, ToRightAssoc[Qs]] => Value.ObjectBuilder[Qs, Void],
  ): ClientEndpoint.WithQueryParams[Is, Qs, Rest, O] =
    params(f(Value.ObjectBuilder[Qs]).result)
}

object ClientEndpoint {
  extension [Bs, O](endpoint: ClientEndpoint[Void || "body" :: DiscriminatedUnion[Bs], O])
    def body[MimeType <: String](using i: MimeType IsCaseOf Bs)(
      body: Value[i.Type],
    ): HttpThunk[MimeType, O] =
      import endpoint.underlying.{method, responseSchema}
      val (path, bodySchema) =
        endpoint.underlying.requestSchema match
          case RequestSchema.WithBody(RequestSchema.ConstantPath(path), schema) =>
            (path, schema)
      HttpThunk(
        method,
        paramsSchema = RequestSchema.Params.ConstantPath(path),
        params = Value.obj,
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
      import endpoint.underlying.{method, responseSchema}
      val paramsSchema: RequestSchema.Params[Qs] =
        ev1.substituteContra(endpoint.underlying.requestSchema) match
          case RequestSchema.Parameterized(params) => params
      HttpThunk(
        method,
        paramsSchema = paramsSchema,
        params = params,
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
