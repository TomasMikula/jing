package jing.openapi.model.client

import jing.openapi.model.*

class ClientEndpoint[I, O](
  underlying: HttpEndpoint[I, O],
) {
  import ClientEndpoint.InputBuilder

  def withInput(in: Value[I]): HttpThunk[O] =
    import underlying.{path, method, requestSchema, responseSchema}
    HttpThunk(path, method, RequestInput(requestSchema, in), responseSchema)

  def withInput[Ps](using ev: I =:= Obj[Ps])(
    f: InputBuilder[Void, ToRightAssoc[Ps, Void]] => InputBuilder[Ps, Void],
  ): HttpThunk[O] =
    val inputValue = ev.substituteContra(f(InputBuilder[Ps]).result)
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

    extension [Acc, B, Rest](b: InputBuilder[Acc, "body" :: B || Rest])
      def body(body: Value[B]): InputBuilder[Acc || "body" :: B, Rest] =
        b.set("body", body)

    extension [Acc](b: InputBuilder[Acc, Void])
      def result: Value[Obj[Acc]] =
        Value.result(b)
  }
}
