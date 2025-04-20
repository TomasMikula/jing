package jing.openapi.model

/** Schema of request data, i.e. request parameters and request body.
 *
 * @tparam Is named list of request inputs, separated by [[||]]
 */
sealed trait RequestSchema[Is]

object RequestSchema {
  sealed trait ParamsOpt[Is] extends RequestSchema[Is]

  case class WithBody[Ps, B](
    params: ParamsOpt[Ps],
    body: BodySchema.NonEmpty[B],
  ) extends RequestSchema[Ps || "body" :: B]

  case object NoParams extends ParamsOpt[Void]

  case class Parameterized[Ps](
    params: Schema[Obj[Ps]],
  ) extends ParamsOpt[Void || "params" :: Obj[Ps]]
}
