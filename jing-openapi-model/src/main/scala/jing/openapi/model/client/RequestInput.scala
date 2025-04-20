package jing.openapi.model.client

import jing.openapi.model.*

/** Request data, i.e. request parameters and request body.
 *
 * @tparam MimeType limits the possible MIME types of request body.
 *   Example: `"application/json" | "application/x-www-form-urlencoded"`
 * @tparam Is list of request inputs, separated by [[||]]
 */
enum RequestInput[+MimeType, Is] {
  case NoInput extends RequestInput[Nothing, Void]

  case Params[Ps](value: Value[Obj[Ps]]) extends RequestInput[Nothing, Void || "params" :: Obj[Ps]]

  case BodyOnly[MimeType, B](
    value: RequestInput.Body[MimeType, B],
  ) extends RequestInput[MimeType, Void || "body" :: B]

  case ParamsAndBody[Ps, MimeType, B](
    params: Value[Obj[Ps]],
    body: RequestInput.Body[MimeType, B],
  ) extends RequestInput[MimeType, Void || "params" :: Obj[Ps] || "body" :: B]

  def queryParams: Option[Map[String, Value[?]]] =
    this match
      case NoInput =>
        None
      case Params(value) =>
        Some(Value.toMap(value))
      case BodyOnly(_) =>
        None
      case ParamsAndBody(params, body) =>
        Some(Value.toMap(params))

}

object RequestInput {

  enum Body[MimeType, B]:
    case MimeVariant[MimeType, BodyType, SchemaVariants](
      schema: BodySchema.NonEmpty[DiscriminatedUnion[SchemaVariants]],
      variantSelector: (MimeType IsCaseOf SchemaVariants) { type Type = BodyType },
      value: Value[BodyType],
    ) extends Body[MimeType, DiscriminatedUnion[SchemaVariants]]

  object Body {
    def apply[B](
      schema: BodySchema.NonEmpty[B],
      value: Value[B],
    ): Body[?, B] =
      schema match
        case vs: BodySchema.Variants[cases] =>
          (value: Value[DiscriminatedUnion[cases]])
            .handleDiscriminatedUnion { [Label <: String, A] => (i, va) =>
              Body.MimeVariant[Label, A, cases](vs, i, va)
            }
  }

  import RequestInput.*

  def bodyOnly[SchemaVariants, MimeType, BodyType](
    schema: BodySchema.NonEmpty[DiscriminatedUnion[SchemaVariants]],
    variantSelector: (MimeType IsCaseOf SchemaVariants) { type Type = BodyType },
    value: Value[BodyType],
  ): RequestInput[MimeType, Void || "body" :: DiscriminatedUnion[SchemaVariants]] =
    BodyOnly(Body.MimeVariant(schema, variantSelector, value))

}
