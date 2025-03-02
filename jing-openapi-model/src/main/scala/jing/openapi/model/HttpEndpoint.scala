package jing.openapi.model

case class HttpEndpoint[I, O](
  path: String,
  method: HttpMethod,
  requestSchema: RequestSchema[I],
  responseSchema: Schema[O],
) {
  def withInput(in: Value[I]): HttpThunk[O] =
    HttpThunk(path, method, RequestInput(requestSchema, in), responseSchema)
}
