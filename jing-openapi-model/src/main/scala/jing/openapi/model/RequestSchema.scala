package jing.openapi.model

/** Schema of request data, i.e. request parameters and request body. */
enum RequestSchema[I] {
  case NoInput extends RequestSchema[Obj[Void]]

  case Params[Ps](schema: Schema[Obj[Ps]]) extends RequestSchema[Obj[Void || "params" :: Obj[Ps]]]

  case Body[B](schema: BodySchema.NonEmpty[B]) extends RequestSchema[Obj[Void || "body" :: B]]

  case ParamsAndBody[Ps, B](
    params: Schema[Obj[Ps]],
    body: BodySchema.NonEmpty[B],
  ) extends RequestSchema[Obj[Void || "params" :: Obj[Ps] || "body" :: B]]
}
