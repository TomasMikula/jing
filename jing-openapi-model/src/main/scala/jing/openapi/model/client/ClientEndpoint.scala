package jing.openapi.model.client

import jing.openapi.model.*

class ClientEndpoint[Is, O](
  underlying: HttpEndpoint[Is, O],
) {
  import ClientEndpoint.*

  def withInput(in: Value[Obj[Is]]): HttpThunk[O] =
    import underlying.{path, method, requestSchema, responseSchema}
    HttpThunk(path, method, RequestInput(requestSchema, in), responseSchema)

  def withInput(
    f: InputBuilder[Void, ToRightAssoc[Is, Void]] => InputBuilder[Is, Void],
  ): HttpThunk[O] =
    val inputValue = f(InputBuilder[Is]).result
    withInput(inputValue)

  def queryParams[Qs, Rest](using ev: ToRightAssoc[Is, Void] =:= ("params" :: Qs || Rest))(
    params: Value[Qs],
  ): ClientEndpoint.RequestBuilder[Is, Void || "params" :: Qs, Rest, O] =
    val inputBuilder: InputBuilder[Void, "params" :: Qs || Rest] =
      ev.substituteCo(InputBuilder[Is])
    RequestBuilder(this, inputBuilder.queryParams(params))
}

object ClientEndpoint {
  opaque type InputBuilder[Acc, Remaining] = Value.ObjectBuilder[Acc, Remaining]

  object InputBuilder {
    private[ClientEndpoint] def apply[Ps]: InputBuilder[Void, ToRightAssoc[Ps, Void]] =
      Value.ObjectBuilder[Ps]

    extension [Acc, Qs, Rest](b: InputBuilder[Acc, "params" :: Qs || Rest])
      def queryParams(params: Value[Qs]): InputBuilder[Acc || "params" :: Qs, Rest] =
        b.set("params", params)

    extension [Acc, Bs, Rest](b: InputBuilder[Acc, "body" :: DiscriminatedUnion[Bs] || Rest])
      def body[MimeType <: String](using ev: MimeType IsCaseOf Bs)(
        body: Value[ev.Type],
      ): InputBuilder[Acc || "body" :: DiscriminatedUnion[Bs], Rest] =
        b.set("body", Value.discriminatedUnion[MimeType, ev.Type, Bs](ev, body))

    extension [Acc](b: InputBuilder[Acc, Void])
      def result: Value[Obj[Acc]] =
        Value.result(b)
  }

  extension [Bs, O](endpoint: ClientEndpoint[Void || "body" :: DiscriminatedUnion[Bs], O])
    def body[MimeType <: String](using i: MimeType IsCaseOf Bs)(
      body: Value[i.Type],
    ): HttpThunk[O] =
      val input: Value[Obj[Void || "body" :: DiscriminatedUnion[Bs]]] =
        Value.obj.set("body", Value.discriminatedUnion(i, body))
      endpoint.withInput(input)

  class RequestBuilder[Is, Acc, Remaining, O](
    endpoint: ClientEndpoint[Is, O],
    inputBuilder: InputBuilder[Acc, Remaining],
  ) {
    // TODO: queryParams

    // TODO: body

    def toRequest(using ev1: Acc =:= Is, ev2: Remaining =:= Void): HttpThunk[O] =
      val input: Value[Obj[Is]] =
        ev1.substituteCo[[x] =>> Value[Obj[x]]](ev2.substituteCo(inputBuilder).result)
      endpoint.withInput(input)

    def runAgainst(using Acc =:= Is, Remaining =:= Void)(
      apiBaseUrl: String,
    )(using
      client: Client,
    ): client.Response[O] = {
      toRequest.runAgainst(apiBaseUrl)
    }
  }
}
