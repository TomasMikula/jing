package jing.openapi.examples

import jing.openapi.client.default.given
import jing.openapi.model.Value
import jing.openapi.model.Value.obj

object TestApp extends App {

  val api = jing.openapi("https://petstore3.swagger.io/api/v3/openapi.json")

  import api.schemas.Pet

  private def petRewrap(x: Value[Pet]): Value[Pet] =
    x match
      case Pet(y) => Pet(y)

  println(Pet.schema)
  println()

  val result =
    api
      .paths
      .`/pet/findByStatus`
      .Get
      .withInput(
        obj
          .set("status", "available")
      )
      .runAgainst("https://petstore3.swagger.io/api/v3")

  println(result)
}
