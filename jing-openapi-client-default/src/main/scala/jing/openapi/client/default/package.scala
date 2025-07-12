package jing.openapi.client.default

import jing.openapi.model.Value
import libretto.lambda.util.Validated

given instance: ClientJdk[Value] =
  new ClientJdk(
    [A] => (va: Value.Lenient[A]) =>
      va.toValue match
        case Validated.Valid(value) =>
          Result.Succeeded(value)
        case Validated.Invalid(errors) =>
          Result.unsupportedSchema(errors)
  )

given instanceLenient: ClientJdk[Value.Lenient] =
  new ClientJdk([A] => Result.Succeeded(_))
