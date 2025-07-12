package jing.openapi.client.default

import jing.openapi.model.Value

given instance: ClientJdk[Value.Lenient] =
  new ClientJdk([A] => Result.Succeeded(_))
