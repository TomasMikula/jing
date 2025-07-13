package jing.openapi.examples.petstore.client

import jing.openapi.client.default.Result.{Failed, Succeeded}
import jing.openapi.client.default.{Response, instance}
import jing.openapi.examples.petstore.api
import jing.openapi.model.*
import jing.openapi.model.Value.{arr, discriminatedUnion, enm, int64, obj, str}
import jing.openapi.model.client.ClientEndpoint

object TestApp {

  val serverUrl = "https://petstore3.swagger.io/api/v3"
  // val serverUrl = "http://localhost:8080"

  import api.schemas.{Category, Pet}

  def main(args: Array[String]): Unit =
    // Create a pet
    // ------------
    // POST request with JSON body
    val createPetResponse =
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
        .assertSuccess("Creating pet failed")

    println(s"Create pet response: ${createPetResponse.show}")

    // obtain pet id from the response
    val petId =
      val Pet(pet) = createPetResponse.assertStatus["200"].assertCase["application/json"]
      pet.props["id"] match
        case Some(id) => id.longValue
        case None => sys.error("Server did not return pet id of a newly created pet")

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
        .params.fromNamedTuple:
          ( petId = petId   // path parameter
          , name = "Muffin" // query parameter
          , status = "sold" // query parameter
          )
        .runAgainst(serverUrl)
        .assertSuccess("Updating pet failed") match
          case resp =>
            println(s"Update pet response: ${resp.show}")

    // Find available pets
    // -------------------
    // GET request with a query parameter
    api
      .paths
      .`/pet/findByStatus`
      .Get
      .as[ClientEndpoint]
      .params.fromNamedTuple:
        ( status = enm("available")
        )
      .runAgainst(serverUrl)
      .assertSuccess("findByStatus failed")
      .assertStatus["200"]
      .assertCase["application/json"] match
        case body =>
          println(s"findByStatus result: ${body.show}")

    // Find pets with the given tags
    // -----------------------------
    // GET request with an array-typed query parameter
    api
      .paths
      .`/pet/findByTags`
      .Get
      .as[ClientEndpoint]
      .params.fromNamedTuple:
        ( tags = arr("tag1", "tag2")
        )
      .runAgainst(serverUrl)
      .assertSuccess("findByTags failed")
      .assertStatus["200"]
      .assertCase["application/json"] match
        case body =>
          println(s"findByTags result: ${body.show}")

  end main
}
