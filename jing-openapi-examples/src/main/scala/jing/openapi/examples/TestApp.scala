package jing.openapi.examples

import jing.openapi.client.default.Result.{Failed, Succeeded}
import jing.openapi.client.default.{Response, instance}
import jing.openapi.model.*
import jing.openapi.model.Value.{arr, discriminatedUnion, enm, int64, obj, objFromTuple, str}
import jing.openapi.model.client.ClientEndpoint

object TestApp extends App {

  val api = jing.openapi("https://petstore3.swagger.io/api/v3/openapi.json")

  val serverUrl = "https://petstore3.swagger.io/api/v3"
  // val serverUrl = "http://localhost:8080"

  // loading from local file works, too, although resolving a relative path
  // currently breaks Metals suggestions (https://github.com/scalameta/metals/issues/7460)
  //
  // val api = jing.openapi("petstore3.json") // relative to this source file

  import api.schemas.{Category, Pet}

  // Create a pet
  // ------------
  // POST request with JSON body
  val postResult =
    api
      .paths
      .`/pet`
      .Post
      .as[ClientEndpoint]
      .body["application/json"]({

       // constructing Obj-ects using builder pattern
        Pet(obj(_
          .set("id", 12345L)
          .set("name", "Cookie")
          .set("category", Category(obj(_.set("id", 1L).skip("name"))))
          .set("photoUrls", arr(str("https://cookie.com/pic.jpg")))
          .skip("tags")
          .set("status", "available")
        ))

        // constructing Obj-ects from named tuples
        Pet(objFromTuple(_(
          id = int64(12345L),
          name = str("Cookie"),
          category = Category(objFromTuple(_(id = int64(1L), name = None))),
          photoUrls = arr(str("https://cookie.com/pic.jpg")),
          tags = None,
          status = enm("available"),
        )))

      })
      .runAgainst(serverUrl)

  println(postResult.map(_.show))

  // Update the pet's name and status
  // --------------------------------
  // POST request with path and query parameters.
  // Notice uniform treatment of path and query parameters.
  val updateResult =
    api
      .paths
      .`/pet/{petId}`
      .Post
      .as[ClientEndpoint]
      .params(_
        .set("petId", 12345L)  // path parameter
        .set("name", "Muffin") // query parameter
        .set("status", "sold") // query parameter
      )
      .runAgainst(serverUrl)

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
      .as[ClientEndpoint]
      .params(_
        .set("status", "available")
      )
      .runAgainst(serverUrl)

  println()
  findResult match
    case Failed(e) =>
      println(s"Failed with: $e")
    case Succeeded(Response.Accurate(value)) =>
      val body: Value.Lenient[Arr[Pet]] =
        value
          .assertCase["200"]
          .assertCase["application/json"]
      println(body.show)
    case Succeeded(resp @ Response.WithExtraneousBody(_, body)) =>
      println(s"${resp.statusCode}: $body")

  // Find pets with the given tags
  // -----------------------------
  // GET request with an array-typed query parameter
  val findByTagsResult =
    api
      .paths
      .`/pet/findByTags`
      .Get
      .as[ClientEndpoint]
      .params(_
        .set("tags", arr("tag1", "tag2")),
      )
      .runAgainst(serverUrl)

  println()
  findByTagsResult match
    case Failed(e) =>
      println(s"Failed with: $e")
    case Succeeded(Response.Accurate(value)) =>
      val body: Value.Lenient[Arr[Pet]] =
        value
          .assertCase["200"]
          .assertCase["application/json"]
      println(body.show)
    case Succeeded(resp @ Response.WithExtraneousBody(_, body)) =>
      println(s"${resp.statusCode}: $body")
}
