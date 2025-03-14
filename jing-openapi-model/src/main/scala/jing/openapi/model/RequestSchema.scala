package jing.openapi.model

/** Schema of request data, i.e. request parameters and request body. */
enum RequestSchema[I] {
  case NoInput extends RequestSchema[Unit]

  case Params[Ps](schema: Schema[Obj[Ps]]) extends RequestSchema[Obj[Ps]]

  case Body(schema: BodySchema[I])

  case ParamsAndBody[Ps, B](
    params: Schema[Obj[Ps]],
    body: BodySchema[B],
  ) extends RequestSchema[Obj[{} || "params" :: Obj[Ps] || "body" :: B]]
}
