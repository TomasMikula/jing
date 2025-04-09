package jing.openapi.examples

import jing.openapi.client.default.given
import jing.openapi.client.default.Result.{Failed, Succeeded}
import jing.openapi.model.*
import jing.openapi.model.Value.obj

object TestApp extends App {

  val api = jing.openapi("https://petstore3.swagger.io/api/v3/openapi.json")
  import api.schemas.Pet

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

  result match
    case Failed(e) =>
      println(s"Failed with: $e")
    case Succeeded(value) =>
      val d = value.discriminator
      val body: Value[Arr[Pet]] =
        value
          .assertCase["200"]
          .assertCase["application/json"]
      println(body.show)

}
