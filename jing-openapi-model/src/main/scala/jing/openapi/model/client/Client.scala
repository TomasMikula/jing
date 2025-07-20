package jing.openapi.model.client

trait Client {
  type Response[T]

  type SupportedMimeType

  def runRequest[O](
    baseUrl: String,
    req: HttpRequest[SupportedMimeType, O],
  ): Response[O]
}
