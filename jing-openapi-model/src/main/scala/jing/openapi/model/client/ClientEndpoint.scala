package jing.openapi.model.client

import jing.openapi.model.*

class ClientEndpoint[I, O](
  underlying: HttpEndpoint[I, O],
) {
  def withInput(in: Value[I]): HttpThunk[O] =
    import underlying.{path, method, requestSchema, responseSchema}
    HttpThunk(path, method, RequestInput(requestSchema, in), responseSchema)
}
