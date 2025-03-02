package jing.openapi.model

trait Client {
  type Result[T]

  def runRequest[O](
    baseUrl: String,
    req: HttpThunk[O],
  ): Result[O]
}
