package jing.openapi

import jing.openapi.model.EndpointList
import org.scalatest.funsuite.AnyFunSuite
import scala.NamedTuple.NamedTuple

class SwaggerToScalaAstTest extends AnyFunSuite {

  test("empty spec") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: Simple API overview
        version: 2.0.0
      paths: {}
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time check that endpointList is empty
    api.endpointList : EndpointList[Void, NamedTuple[EmptyTuple, EmptyTuple]]
  }

}
