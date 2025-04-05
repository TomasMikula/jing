package jing.openapi.model

trait Client {
  type Response[T]

  def runRequest[O](
    baseUrl: String,
    req: HttpThunk[O],
  ): Response[O]
}
