package jing.openapi

import jing.openapi.model.{EndpointList, Schema}
import org.scalatest.funsuite.AnyFunSuite

import scala.NamedTuple.NamedTuple

class SwaggerToScalaAstTest extends AnyFunSuite {

  test("empty spec") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: Empty API
        version: 2.0.0
      paths: {}
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time check that endpointList is empty
    api.endpointList : EndpointList[Void, NamedTuple[EmptyTuple, EmptyTuple]]
  }

  test("nullable enums") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: Empty API
        version: 2.0.0
      paths: {}
      components:
        schemas:
          FooBar:
            title: FooBar
            type: string
            enum:
              - Foo
              - Bar
              - null
          OneTwo:
            title: OneTwo
            type: integer
            format: int32
            enum:
              - 1
              - 2
              - null
          OneTwo64:
            title: OneTwo64
            type: integer
            format: int64
            enum:
              - 1
              - 2
              - null
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    api.schemas.FooBar.schema match
      case Schema.Proper(value) =>
        fail("expected unsupported null in string enum")
      case Schema.Unsupported(message) =>
        assert(message.value == "null not supported as an enum case of strings. Got: Foo,Bar,null")

    api.schemas.OneTwo.schema match
      case Schema.Proper(value) =>
        fail("expected unsupported null in int32 enum")
      case Schema.Unsupported(message) =>
        assert(message.value == "null not supported as an enum case of 32-bit integers. Got: 1,2,null")

    api.schemas.OneTwo64.schema match
      case Schema.Proper(value) =>
        fail("expected unsupported null in int64 enum")
      case Schema.Unsupported(message) =>
        assert(message.value == "null not supported as an enum case of 64-bit integers. Got: 1,2,null")
  }

}
