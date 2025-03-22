package jing.openapi.examples

import jing.openapi.client.jdk.given
import jing.openapi.model.Value
import jing.openapi.model.Value.obj
import scala.reflect.Selectable.reflectiveSelectable

object TestApp extends App {

  val api = jing.openapi("https://petstore3.swagger.io/api/v3/openapi.json")

  import api.schemas.Pet

  private def petRewrap(x: Value[Pet]): Value[Pet] =
    Pet.unapply(x) match // structural unapply not supported
      case Some(y) => Pet(y)

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
