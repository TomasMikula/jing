package jing.openapi.examples.petstore.client

import jing.openapi.client.default.Result.{Failed, Succeeded}
import jing.openapi.client.default.{Response, instance}
import jing.openapi.examples.petstore.api
import jing.openapi.model.*
import jing.openapi.model.Value.{arr, discriminatedUnion, enm, int64, obj, str}
import jing.openapi.model.client.ClientEndpoint

object TestApp extends App {

  val serverUrl = "https://petstore3.swagger.io/api/v3"
  // val serverUrl = "http://localhost:8080"

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
        Pet(obj.builder(_
          .set("id", 12345L) // XXX: petstore3.swagger.io does require id when creating a pet 🤦
          .set("name", "Cookie")
          .skip("category")
          .set("photoUrls", arr(str("https://cookie.com/pic.jpg")))
          .skip("tags")
          .set("status", "available")
        ))

        // constructing Obj-ects from named tuples
        Pet(obj(_(
          id = 12345L, // XXX: petstore3.swagger.io does require id when creating a pet 🤦
          name = "Cookie",
          category = None,
          photoUrls = arr("https://cookie.com/pic.jpg"),
          tags = None,
          status = enm("available"),
        )))

      })
      .runAgainst(serverUrl)

  // obtain pet id from the response
  val petId =
    postResult match
      case Failed(e) =>
        println(s"Create pet failed with: $e")
        -1
      case Succeeded(resp) =>
        println(s"Create pet result: ${resp.show}")
        val Pet(pet) = resp.assertStatus.apply["200"].assertCase["application/json"]
        pet.props["id"] match
          case Some(id) => id.longValue
          case None => -1

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
        .set("petId", petId)   // path parameter
        .set("name", "Muffin") // query parameter
        .set("status", "sold") // query parameter
      )
      .runAgainst(serverUrl)

  println()
  println(s"Update pet result: ${updateResult.map(_.show)}")

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
      println(s"findByStatus failed with: $e")
    case Succeeded(resp) =>
      val body = resp.assertStatus["200"].assertCase["application/json"]
      println(s"findByStatus result: ${body.show}")

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
      println(s"findByTags failed with: $e")
    case Succeeded(resp) =>
      val body = resp.assertStatus["200"].assertCase["application/json"]
      println(s"findByTags result: ${body.show}")
}
