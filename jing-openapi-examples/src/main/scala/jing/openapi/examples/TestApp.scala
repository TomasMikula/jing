package jing.openapi.examples

import jing.openapi.client.default.{instance as DefaultClient}
import jing.openapi.client.default.Result.{Failed, Succeeded}
import jing.openapi.model.*
import jing.openapi.model.Value.{arr, discriminatedUnion, obj, str}

object TestApp extends App {

  val api = jing.openapi("https://petstore3.swagger.io/api/v3/openapi.json")
  import api.schemas.{Category, Pet}

  // Create a pet
  // ------------
  // POST request with JSON body
  val postResult =
    api
      .paths
      .`/pet`
      .Post
      .interpret(using DefaultClient)
      .body["application/json"](
        Pet(obj(_
          .set("id", 12345L)
          .set("name", "Cookie")
          .set("category", Category(obj(_.skip("id").set("name", "cuties"))))
          .set("photoUrls", arr(str("https://cookie.com/pic.jpg")))
          .skip("tags")
          .set("status", "available")
        ))
      )
      .runAgainst("https://petstore3.swagger.io/api/v3")

  println(postResult.map(_.show))

  // Update the pet's status
  // -----------------------
  // POST request with path and query parameters.
  // Notice uniform treatment of path and query parameters.
  val updateResult =
    api
      .paths
      .`/pet/{petId}`
      .Post
      .interpret(using DefaultClient)
      .params(_
        .set("petId", 12345L)  // path parameter
        .set("name", "Muffin") // query parameter
        .set("status", "sold") // query parameter
      )
      .runAgainst("https://petstore3.swagger.io/api/v3")

  println()
  println(updateResult.map(_.show))

  // Find available pets
  // -------------------
  // GET request with a query parameter
  val findResult =
    api
      .paths
      .`/pet/findByStatus`
      .Get
      .interpret(using DefaultClient)
      .params(_
        .set("status", "available")
      )
      .runAgainst("https://petstore3.swagger.io/api/v3")

  println()
  findResult match
    case Failed(e) =>
      println(s"Failed with: $e")
    case Succeeded(value) =>
      val body: Value.Lenient[Arr[Pet]] =
        value
          .assertCase["200"]
          .assertCase["application/json"]
      println(body.show)

  // Find pets with the given tags
  // -----------------------------
  // GET request with an array-typed query parameter
  val findByTagsResult =
    api
      .paths
      .`/pet/findByTags`
      .Get
      .interpret(using DefaultClient)
      .params(_
        .set("tags", arr("tag1", "tag2")),
      )
      .runAgainst("https://petstore3.swagger.io/api/v3")

  println()
  findByTagsResult match
    case Failed(e) =>
      println(s"Failed with: $e")
    case Succeeded(value) =>
      val body: Value.Lenient[Arr[Pet]] =
        value
          .assertCase["200"]
          .assertCase["application/json"]
      println(body.show)
}
