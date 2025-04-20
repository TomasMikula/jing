package jing.openapi.model

enum Body[+MimeType, B]:
  case MimeVariant[MimeType, BodyType, SchemaVariants](
    schema: BodySchema.NonEmpty[DiscriminatedUnion[SchemaVariants]],
    variantSelector: (MimeType IsCaseOf SchemaVariants) { type Type = BodyType },
    value: Value[BodyType],
  ) extends Body[MimeType, DiscriminatedUnion[SchemaVariants]]

object Body {
  def apply[SchemaVariants, MimeType, BodyType](
    schema: BodySchema.NonEmpty[DiscriminatedUnion[SchemaVariants]],
    variantSelector: (MimeType IsCaseOf SchemaVariants) { type Type = BodyType },
    value: Value[BodyType],
  ): Body[MimeType, DiscriminatedUnion[SchemaVariants]] =
    Body.MimeVariant(schema, variantSelector, value)
}
