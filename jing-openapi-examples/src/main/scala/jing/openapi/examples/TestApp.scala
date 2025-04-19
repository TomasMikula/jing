package jing.openapi.examples

import jing.openapi.client.default.{instance as DefaultClient}
import jing.openapi.client.default.Result.{Failed, Succeeded}
import jing.openapi.model.*
import jing.openapi.model.Value.{arr, discriminatedUnion, obj, str}

object TestApp extends App {

  val api = jing.openapi("https://petstore3.swagger.io/api/v3/openapi.json")
  import api.schemas.{Category, Pet}

  // Create a pet
  val postResult =
    api
      .paths
      .`/pet`
      .Post
      .interpret(using DefaultClient)
      .withInput(_
        .body(
          discriminatedUnion:
            _.pick["application/json"](
              Pet(obj(_
                .set("id", 12345L)
                .set("name", "Cookie")
                .set("category", Category(obj(_.skip("id").set("name", "cuties"))))
                .set("photoUrls", arr(str("https://cookie.com/pic.jpg")))
                .skip("tags")
                .set("status", "available")
              ))
            )
        )
      )
      .runAgainst("https://petstore3.swagger.io/api/v3")

  println(postResult.map(_.show))

  // Find available pets
  val findResult =
    api
      .paths
      .`/pet/findByStatus`
      .Get
      .interpret(using DefaultClient)
      .withInput(_
        .queryParams(
          obj.set("status", "available")
        )
      )
      .runAgainst("https://petstore3.swagger.io/api/v3")

  findResult match
    case Failed(e) =>
      println(s"Failed with: $e")
    case Succeeded(value) =>
      val d = value.discriminator
      val body: Value.Lenient[Arr[Pet]] =
        value
          .assertCase["200"]
          .assertCase["application/json"]
      println(body.show)

}
