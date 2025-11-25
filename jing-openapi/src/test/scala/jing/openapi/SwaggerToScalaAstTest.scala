package jing.openapi

import jing.openapi.model.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite

import scala.NamedTuple.NamedTuple

class SwaggerToScalaAstTest extends AnyFunSuite with Inside {

  test("empty spec") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
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
      openapi: 3.1.0
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
      openapi: 3.1.0
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
      openapi: 3.1.0
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
    api.schemas.Foo.from : (Value[Oops["abcd of type String not supported as an enum case of 64-bit integers. Got: 9223372036854775807,abcd,-9223372036854775808"]] => Value[api.schemas.Foo])
  }

  test("string enum with integer and boolean literals") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
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

  test("boolean enums") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
      info:
        title: boolean enums
        version: 1.0.0
      paths: {}
      components:
        schemas:
          TrueType:
            type: boolean
            enum: [true]
          FalseType:
            type: boolean
            enum: [false]
          BooleanType:
            type: boolean
            enum: [true, false]
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time checks that schemas have the expected definitions
    api.schemas.TrueType.from : (Value[Enum[Bool, Void || true]] => Value[api.schemas.TrueType])
    api.schemas.FalseType.from : (Value[Enum[Bool, Void || false]] => Value[api.schemas.FalseType])
    api.schemas.BooleanType.from : (Value[Enum[Bool, Void || true || false]] => Value[api.schemas.BooleanType])
  }

  test("boolean enum with non-boolean literals fails gracefully") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
      info:
        title: boolean enum with non-boolean literals fails gracefully
        version: 1.0.0
      paths: {}
      components:
        schemas:
          Foo:
            type: boolean
            enum: [true, abcd, "true", 123]
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time check that Foo has the expected definition
    // Note: The Swagger parser is just plain stupid: gives us false in place of "abcd"
    api.schemas.Foo.from : (Value[Oops["123 of type Integer not supported as an enum case of booleans. Got: true,false,true,123"]] => Value[api.schemas.Foo])
  }

  test("const of primitives") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
      info:
        title: const of primitives
        version: 1.0.0
      paths: {}
      components:
        schemas:
          Foo:
            type: boolean
            const: true
          Bar:
            type: integer
            format: int32
            const: 123
          Baz:
            type: integer
            format: int64
            const: 456
          Qux:
            type: string
            const: "qux"
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time checks that schemas have the expected definitions
    api.schemas.Foo.from : (Value[Const[true]] => Value[api.schemas.Foo])
    api.schemas.Bar.from : (Value[Const[123]] => Value[api.schemas.Bar])
    api.schemas.Baz.from : (Value[Const[456L]] => Value[api.schemas.Baz])
    api.schemas.Qux.from : (Value[Const["qux"]] => Value[api.schemas.Qux])
  }

  test("combination of const and enum") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
      info:
        title: combination of const and enum
        version: 1.0.0
      paths: {}
      components:
        schemas:
          Foo:
            type: integer
            format: int32
            const: 2
            enum: [1, 2, 3]
          Bar:
            type: string
            enum: ["foo", "baz"]
            const: "bar"
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time checks that schemas have the expected definitions
    api.schemas.Foo.from : (Value[Const[2]] => Value[api.schemas.Foo])
    api.schemas.Bar.from : (Value[Oops["Constant 'bar' is not one of the enum cases: foo,baz"]] => Value[api.schemas.Bar])
  }

  test("object with empty or missing property list") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
      info:
        title: object with empty property list
        version: 1.0.0
      paths: {}
      components:
        schemas:
          Foo:
            type: object
            properties: {}
          Bar:
            type: object
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    // compile-time checks that schemas have the expected definitions
    api.schemas.Foo.from : (Value[Obj[Void]] => Value[api.schemas.Foo])
    api.schemas.Bar.from : (Value[Obj[Void]] => Value[api.schemas.Bar])
  }

  test("nullable enums") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
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
      openapi: 3.1.0
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
      openapi: 3.1.0
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
      openapi: 3.1.0
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
                const: "Dog"
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

  test("oneOf with discriminator mapping") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
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
              mapping:
                cat: "#/components/schemas/Cat"
                dog: "#/components/schemas/Dog"
          Cat:
            type: object
            properties:
              species:
                type: string
                enum: [cat]
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
                const: dog
              name:
                type: string
            required:
              - species
              - name
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    import api.schemas.{Animal, Cat, Dog}

    // check that Animal constructor and deconstructor have the expected types
    Animal.from        : (Value[DiscriminatedUnion["cat" :: Cat || "dog" :: Dog]] => Value[Animal])
    Animal.deconstruct : (Value[Animal] => Value[DiscriminatedUnion["cat" :: Cat || "dog" :: Dog]])

    inside(Animal.schema):
      case Schema.Proper(value) =>
        inside(value):
          case SchemaMotif.OneOf(discriminatorProperty, schemas) =>
            assert(discriminatorProperty == "species")

            inside(schemas.getOption("cat")):
              case Some(found) =>
                val catSchema = found.value._2
                assert(catSchema == Cat.schema)

            inside(schemas.getOption("dog")):
              case Some(found) =>
                val dogSchema = found.value._2
                assert(dogSchema == Dog.schema)
  }

  test("oneOf with discriminator mapping not matching reality fails gracefully") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
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
              mapping:
                kitten: "#/components/schemas/Cat"
                puppy: "#/components/schemas/Dog"
          Cat:
            type: object
            properties:
              species:
                type: string
                enum: [cat]
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
                const: dog
              name:
                type: string
            required:
              - species
              - name
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    import api.schemas.{Animal, Cat, Dog}

    // check that Animal constructor and deconstructor have the expected types
    Animal.from : (Value[Oops["Mapping of \"species\": \"kitten\" refers to schema Cat, which, however, defines \"species\": \"cat\". Mapping of \"species\": \"puppy\" refers to schema Dog, which, however, defines \"species\": \"dog\". Mapping is missing entry for \"species\": \"cat\", defined by oneOf case 1 (Cat). Mapping is missing entry for \"species\": \"dog\", defined by oneOf case 2 (Dog)."]] => Value[Animal])
  }

  test("path with a param strictly inside a segment") {
    inline val openapiYaml =
      """
      openapi: 3.1.0
      info:
        title: sub-segment path parameter
        version: 2.0.0
      paths:
        /a/v{major}.{minor}/c:
          get:
            parameters:
              - name: major
                in: path
                required: true
                schema:
                  type: integer
                  format: int32
              - name: minor
                in: path
                required: true
                schema:
                  type: integer
                  format: int32
            responses:
              "204":
                description: No Content
      """

    val api = jing.openapi.inlineYaml(openapiYaml)

    api.paths.`/a/v{major}.{minor}/c`.Get.requestSchema match
      case RequestSchema.Parameterized(params) =>
        // check that `params` has the expected type
        params : RequestSchema.Params.Proper[Void || "major" :: Int32 || "minor" :: Int32]

        inside(params):
          case RequestSchema.Params.ParameterizedPath(pathTemplate) =>
            val path = pathTemplate.instantiate((major = 2, minor = 6))
            assert(path == "/a/v2.6/c")
  }

}
