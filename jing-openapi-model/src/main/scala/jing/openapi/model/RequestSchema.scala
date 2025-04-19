package jing.openapi.model

/** Schema of request data, i.e. request parameters and request body.
 *
 * @tparam Is named list of request inputs, separated by [[||]]
 */
enum RequestSchema[Is] {
  case NoInput extends RequestSchema[Void]

  case Params[Ps](schema: Schema[Obj[Ps]]) extends RequestSchema[Void || "params" :: Obj[Ps]]

  case Body[B](schema: BodySchema.NonEmpty[B]) extends RequestSchema[Void || "body" :: B]

  case ParamsAndBody[Ps, B](
    params: Schema[Obj[Ps]],
    body: BodySchema.NonEmpty[B],
  ) extends RequestSchema[Void || "params" :: Obj[Ps] || "body" :: B]
}
