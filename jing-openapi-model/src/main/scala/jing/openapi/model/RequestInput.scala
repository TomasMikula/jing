package jing.openapi.model

/** Request data, i.e. request parameters and request body. */
enum RequestInput[I] {
  case NoInput extends RequestInput[Unit]

  case Params[Ps](value: Value[Obj[Ps]]) extends RequestInput[Obj[Ps]]

  case Body(
    schema: BodySchema[I],
    value: Value[I],
  )

  case ParamsAndBody[Ps, B](
    params: Value[Obj[Ps]],
    body: Body[B],
  ) extends RequestInput[Obj[{} || "params" :: Obj[Ps] || "body" :: B]]

  def queryParams: Option[Map[String, Value[?]]] =
    this match
      case NoInput =>
        None
      case Params(value) =>
        Some(Value.toMap(value))
      case Body(_, _) =>
        None
      case ParamsAndBody(params, body) =>
        Some(Value.toMap(params))

}

object RequestInput {
  import RequestInput.*

  def apply[T](
    schema: RequestSchema[T],
    value: Value[T],
  ): RequestInput[T] =
    schema match
      case RequestSchema.NoInput =>
        NoInput
      case ps: RequestSchema.Params[ps] =>
        Params[ps](value)
      case RequestSchema.Body(schema) =>
        Body(schema, value)
      case pb: RequestSchema.ParamsAndBody[ps, b] =>
        import Value.Object.{ObjEmpty, ObjExt}
        value match
          case ObjExt(ObjExt(ObjEmpty, "params", params), "body", body) =>
            ParamsAndBody(params, Body(pb.body, body))

}
