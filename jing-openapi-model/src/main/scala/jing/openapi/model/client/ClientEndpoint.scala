package jing.openapi.model.client

import jing.openapi.model.*

class ClientEndpoint[Is, O](
  underlying: HttpEndpoint[Is, O],
) {
  import ClientEndpoint.InputBuilder

  def withInput(in: Value[Obj[Is]]): HttpThunk[O] =
    import underlying.{path, method, requestSchema, responseSchema}
    HttpThunk(path, method, RequestInput(requestSchema, in), responseSchema)

  def withInput(
    f: InputBuilder[Void, ToRightAssoc[Is, Void]] => InputBuilder[Is, Void],
  ): HttpThunk[O] =
    val inputValue = f(InputBuilder[Is]).result
    withInput(inputValue)
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
}
