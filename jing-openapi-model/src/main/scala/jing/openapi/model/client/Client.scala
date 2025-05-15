package jing.openapi.model.client

trait Client {
  type Response[T]

  type SupportedMimeType

  def runRequest[O](
    baseUrl: String,
    req: HttpThunk[SupportedMimeType, O],
  ): Response[O]
}
