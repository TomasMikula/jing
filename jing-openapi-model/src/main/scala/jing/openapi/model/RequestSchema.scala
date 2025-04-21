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

  case class ConstantPath(path: String) extends ParamsOpt[Void]

  case class Parameterized[Ps](
    params: Params.NonEmpty[Ps],
  ) extends ParamsOpt[Void || "params" :: Obj[Ps]]

  sealed trait Params[Ps] {
    def path: String
  }

  object Params {
    case class Empty(path: String) extends Params[Void]

    case class NonEmpty[Ps](
      path: String,
      schema: Schema.Object.NonEmpty[Ps],
    ) extends Params[Ps]
  }
}
