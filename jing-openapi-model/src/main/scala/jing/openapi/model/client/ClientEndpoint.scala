package jing.openapi.model.client

import jing.openapi.model.*

class ClientEndpoint[I, O](
  underlying: HttpEndpoint[I, O],
) {
  def withInput(in: Value[I]): HttpThunk[O] =
    import underlying.{path, method, requestSchema, responseSchema}
    HttpThunk(path, method, RequestInput(requestSchema, in), responseSchema)

  def withInput[Ps](using ev: I =:= Obj[Ps])(
    f: Value.ObjectBuilder[Void, ToRightAssoc[Ps, Void]] => Value.ObjectBuilder[Ps, Void],
  ): HttpThunk[O] =
    val inputValue = ev.substituteContra(f(Value.ObjectBuilder[Ps]).result)
    withInput(inputValue)
}
