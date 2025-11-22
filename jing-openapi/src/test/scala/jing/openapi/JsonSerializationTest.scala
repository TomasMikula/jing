package jing.openapi

import jing.openapi.model.{Schema, Value, ValueCodecJson}
import jing.openapi.model.Value.*
import jing.openapi.model.ValueCodecJson.{DecodeResult, decodeLenient, encode}
import libretto.lambda.util.Validated
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite

class JsonSerializationTest extends AnyFunSuite with Inside {

  private def assertDecode[T](schema: Schema[T], jsonStr: String): Value[T] =
    inside(decodeLenient(schema, jsonStr)):
      case DecodeResult.Succeeded(decoded) =>
        inside(decoded.toValue):
          case Validated.Valid(value) => value

  test("serialization of constants") {
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
    import api.schemas.{Foo, Bar, Baz, Qux}

    val foo = Foo.from(Value.constBool(true))
    val bar = Bar.from(Value.constI32(123))
    val baz = Baz.from(Value.constI64(456L))
    val qux = Qux.from(Value.constStr("qux"))

    // encode
    val fooStr = encode(Foo.schema, foo)
    val barStr = encode(Bar.schema, bar)
    val bazStr = encode(Baz.schema, baz)
    val quxStr = encode(Qux.schema, qux)

    assert(fooStr == "true")
    assert(barStr == "123")
    assert(bazStr == "456")
    assert(quxStr == "\"qux\"")

    // decode
    val foo1 = assertDecode(Foo.schema, fooStr)
    val bar1 = assertDecode(Bar.schema, barStr)
    val baz1 = assertDecode(Baz.schema, bazStr)
    val qux1 = assertDecode(Qux.schema, quxStr)

    assert(foo1 == foo)
    assert(bar1 == bar)
    assert(baz1 == baz)
    assert(qux1 == qux)
  }

  test("serialization of discriminated oneOf") {
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
                const: "Cat"
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

    val dog: Value[Dog] =
      Dog((
        species = enm("Dog"),
        name = "Ivy",
      ))

    val dogAnimal: Value[Animal] =
      Animal.from:
        discriminatedUnion:
          _.pick["Dog"](dog)

    // serialization of a Dog "upcasted" to an Animal should be the same as serialization of the Dog directly
    val dogStr = encode(Dog.schema, dog)
    val animalStr = encode(Animal.schema, dogAnimal)
    assert(animalStr == dogStr)

    // decode dogStr as a Dog
    val decodedDog: Value[Dog] =
      assertDecode(Dog.schema, dogStr)

    // decode dogStr as an Animal
    val decodedAnimal: Value[Animal] =
      assertDecode(Animal.schema, dogStr)

    // extract the Dog out of the decoded Animal
    val dogFromDecodedAnimal =
      Animal.deconstruct(decodedAnimal).assertCase["Dog"]

    // the two decodings of a Dog should be the same
    assert(dogFromDecodedAnimal == decodedDog)
  }

}
