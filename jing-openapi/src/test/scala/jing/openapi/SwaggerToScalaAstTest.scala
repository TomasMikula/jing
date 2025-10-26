package jing.openapi

import jing.openapi.model.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite

import scala.NamedTuple.NamedTuple

class SwaggerToScalaAstTest extends AnyFunSuite with Inside {

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

  test("int64 enum with int32 literals") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: int64 enum with int32 literals
        version: 1.0.0
      paths: {}
      components:
        schemas:
          Foo:
            type: integer
            format: int64
            enum: [1, 2, 3]
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time check that Foo has the expected definition
    api.schemas.Foo.from : (Value[Enum[Int64, Void || 1L || 2L || 3L]] => Value[api.schemas.Foo])
  }

  test("int32 enum with int64 literals fails gracefully") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: int32 enum with int64 literals fails gracefully
        version: 1.0.0
      paths: {}
      components:
        schemas:
          Foo:
            type: integer
            format: int32
            enum: [9223372036854775807, -9223372036854775808]
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time check that Foo has the expected definition
    api.schemas.Foo.from : (Value[Oops["9223372036854775807 of type Long not supported as an enum case of 32-bit integers. Got: 9223372036854775807,-9223372036854775808"]] => Value[api.schemas.Foo])
  }

  test("int64 enum with string literals fails gracefully") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: int64 enum with string literals fails gracefully
        version: 1.0.0
      paths: {}
      components:
        schemas:
          Foo:
            type: integer
            format: int64
            enum: [9223372036854775807, "abcd", -9223372036854775808]
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time check that Foo has the expected definition
    // Note: The "null" in the error message is not ideal, but that's what the Swagger parser gives us instead of "abcd".
    api.schemas.Foo.from : (Value[Oops["null not supported as an enum case of 64-bit integers. Got: 9223372036854775807,null,-9223372036854775808"]] => Value[api.schemas.Foo])
  }

  test("string enum with integer and boolean literals") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: string enum with integer and boolean literals
        version: 1.0.0
      paths: {}
      components:
        schemas:
          Foo:
            type: string
            enum: [abcd, 1, true, 9223372036854775807]
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time check that Foo has the expected definition
    api.schemas.Foo.from : (Value[Enum[Str, Void || "abcd" || "1" || "true" || "9223372036854775807"]] => Value[api.schemas.Foo])
  }

  test("nullable enums") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: Nullable enums
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

  test("parameter $ref") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: Parameter references
        version: 2.0.0
      paths:
        /a/b/c:
          get:
            parameters:
              - $ref: "#/components/parameters/debug"
            responses:
              "204":
                description: No Content
      components:
        parameters:
          debug:
            name: debug
            schema:
              type: boolean
            in: query
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    api.paths.`/a/b/c`.Get.requestSchema match
      case RequestSchema.Parameterized(params) =>
        // just checking that it compiles with the given type annotation
        params : RequestSchema.Params.Proper[Void || "N/A" :? Oops["\'$ref\' not yet supported for parameters: #/components/parameters/debug"]]
  }

  test("recursive schemas report a cycle") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: Mutually recursive schemas
        version: 1.0
      paths: {}
      components:
        schemas:
          Foo:
            type: object
            properties:
              bar:
                $ref: "#/components/schemas/Bar"
          Bar:
            type: object
            properties:
              foo:
                $ref: "#/components/schemas/Foo"
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    api.schemas.Foo.schema match
      case Schema.Proper(SchemaMotif.Object(obj)) =>
        obj.getOpt("bar") match
          case Some(barSchema) =>
            barSchema.value match
              case Schema.Proper(_) =>
                succeed
              case other =>
                fail(s"expected proper Bar schema inside Foo (the cycle shall be reported from inside Bar), got $other")
          case None =>
            fail(s"Foo is missing property bar")
      case other =>
        fail(s"expected Obj-ect schema, got $other")

    api.schemas.Bar.schema match
      case Schema.Proper(SchemaMotif.Object(obj)) =>
        obj.getOpt("foo") match
          case Some(fooSchema) =>
            fooSchema.value match
              case Schema.Unsupported(message) =>
                assert(message.value == "Unsupported recursive schema: Bar -> Foo -> Bar")
              case other =>
                fail(s"expected unsupported recursive schema, got $other")
          case None =>
            fail(s"Bar is missing property foo")
      case other =>
        fail(s"expected Obj-ect schema, got $other")
  }

  test("oneOf with discriminator") {
    inline val openapiYaml =
      """
      openapi: 3.0.0
      info:
        title: oneOf with discriminator
        version: 1.0.0
      paths: {}
      components:
        schemas:
          Animal:
            title: Animal
            oneOf:
              - $ref: "#/components/schemas/Cat"
              - $ref: "#/components/schemas/Dog"
            discriminator:
              propertyName: species
          Cat:
            type: object
            properties:
              species:
                type: string
                enum: ["Cat"]
              name:
                type: string
            required:
              - species
              - name
          Dog:
            type: object
            properties:
              species:
                type: string
                enum: ["Dog"]
              name:
                type: string
            required:
              - species
              - name
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    import api.schemas.{Animal, Cat, Dog}

    // check that Animal constructor and deconstructor have the expected types
    Animal.from        : (Value[DiscriminatedUnion["Cat" :: Cat || "Dog" :: Dog]] => Value[Animal])
    Animal.deconstruct : (Value[Animal] => Value[DiscriminatedUnion["Cat" :: Cat || "Dog" :: Dog]])

    inside(Animal.schema):
      case Schema.Proper(value) =>
        inside(value):
          case SchemaMotif.OneOf(discriminatorProperty, schemas) =>
            assert(discriminatorProperty == "species")

            inside(schemas.getOption("Cat")):
              case Some(found) =>
                val catSchema = found.value._2
                assert(catSchema == Cat.schema)

            inside(schemas.getOption("Dog")):
              case Some(found) =>
                val dogSchema = found.value._2
                assert(dogSchema == Dog.schema)

  }

}
