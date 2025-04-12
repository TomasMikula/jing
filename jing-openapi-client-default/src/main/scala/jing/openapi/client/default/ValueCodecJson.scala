package jing.openapi.client.default

import io.circe.{Json, JsonObject}
import jing.openapi.client.default.Result.schemaViolation
import jing.openapi.model.{||, ::, :?, Arr, Obj, Oops, Schema, Schematic, Value}
import jing.openapi.model.Schematic.{Array, I32, I64, S, B, Object}
import libretto.lambda.util.SingletonType
import scala.util.boundary
import scala.collection.mutable.Stack

object ValueCodecJson {
  def encode[T](schema: Schema[T], value: Value[T]): String =
    val builder = new StringBuilder()
    encode(schema, value, builder)
    builder.toString()

  private def encode[T](schema: Schema[T], value: Value[T], builder: StringBuilder): Unit =
    schema match {
      case Schema.Proper(s) =>
        s match
          case I32() => writeJsonNumber(Value.intValue(value), builder)
          case I64() => writeJsonNumber(Value.longValue(value), builder)
          case S()   => writeJsonString(Value.stringValue(value), builder)
          case B()   => writeJsonBoolean(Value.booleanValue(value), builder)
          case a: Array[schema, t] =>
            summon[T =:= Arr[t]]
            builder += '['
            val elemSchema = a.elem
            val elems = Value.asArray[t](value)
            if (elems.nonEmpty) {
              encode(elemSchema, elems(0), builder)
              for (i <- 1 until elems.size) {
                builder += ','
                encode(elemSchema, elems(i), builder)
              }
            }
            builder += ']'
          case o: Object[schema, props] =>
            builder += '{'
            encodeObjectProps(o, Value.asObject(value), builder)
            builder += '}'

      case Schema.Unsupported(message) =>
        // TODO: make unrepresentable
        throw IllegalArgumentException(s"Providing value of unsupported schema: ${value.show}")
    }

  private def encodeObjectProps[Props](
    schema: Schematic.Object[Schema, Props],
    value: Value.Object[Props],
    builder: StringBuilder,
  ): Boolean =
    schema match
      case Object.Empty() =>
        false
      case s: Object.Snoc[schema, init, pname, ptype] =>
        summon[Props =:= (init || pname :: ptype)]
        val (vInit, vLast) = Value.Object.unsnoc[init, pname, ptype](value)
        val propsWritten = encodeObjectProps(s.init, vInit, builder)
        appendProp(s.ptype, s.pname.value, vLast, propsWritten, builder)
        true
      case s: Object.SnocOpt[schema, init, pname, ptype] =>
        summon[Props =:= (init || pname :? ptype)]
        val (vInit, vLastOpt) = Value.Object.unsnoc[init, pname, ptype](value)
        val propsWritten = encodeObjectProps(s.init, vInit, builder)
        vLastOpt match
          case None =>
            propsWritten
          case Some(vLast) =>
            appendProp(s.ptype, s.pname.value, vLast, propsWritten, builder)
            true

  private def appendProp[T](
    schema: Schema[T],
    key: String,
    value: Value[T],
    needsSep: Boolean,
    builder: StringBuilder,
  ): Unit =
    if (needsSep) builder += ','
    writeJsonString(key, builder)
    builder += ':'
    encode(schema, value, builder)

  private def writeJsonString(k: String, builder: StringBuilder): Unit =
    builder += '"'
    k.foreach { c =>
      if (c == '"')
        builder += '\\' += '"'
      else
        builder += c
    }
    builder += '"'

  private def writeJsonNumber(n: Int, builder: StringBuilder): Unit =
    builder.append(n)

  private def writeJsonNumber(n: Long, builder: StringBuilder): Unit =
    builder.append(n)

  private def writeJsonBoolean(b: Boolean, builder: StringBuilder): Unit =
    builder.append(b)

  def decode[T](schema: Schema[T], json: Json): Result[Value[T]] =
    val jsonLoc = Stack("<root>")
    decodeAt(schema, jsonLoc, json)

  private def decodeAt[T](
    schema: Schema[T],
    jsonLoc: Stack[String],
    json: Json,
  ): Result[Value[T]] =
    schema match {
      case Schema.Proper(s) =>
        s match
          case I32() =>
            json.asNumber match
              case Some(jsonNumber) =>
                jsonNumber.toInt match
                  case Some(n) => Result.Succeeded(Value.int32(n))
                  case None => schemaViolation(s"Expected a 32-bit integer, got ${jsonNumber}. At ${jsonLoc.printLoc}")
              case None =>
                schemaViolation(s"Expected JSON number, got ${json.name}. At ${jsonLoc.printLoc}")

          case I64() =>
            json.asNumber match
              case Some(jsonNumber) =>
                jsonNumber.toLong match
                  case Some(n) => Result.Succeeded(Value.int64(n))
                  case None => schemaViolation(s"Expected a 64-bit integer, got ${jsonNumber}. At ${jsonLoc.printLoc}")
              case None =>
                schemaViolation(s"Expected JSON number, got ${json.name}. At ${jsonLoc.printLoc}")

          case S() =>
            json.asString match
              case Some(s) => Result.Succeeded(Value.str(s))
              case None => schemaViolation(s"Expected JSON string, got ${json.name}. At ${jsonLoc.printLoc}")

          case B() =>
            json.asBoolean match
              case Some(b) => Result.Succeeded(Value.bool(b))
              case None => schemaViolation(s"Expected JSON boolean, got ${json.name}. At ${jsonLoc.printLoc}")

          case Array(elemSchema) =>
            json.asArray match
              case Some(jsonElems) => decodeArray(elemSchema, jsonLoc, jsonElems)
              case None => schemaViolation(s"Expected JSON array, got ${json.name}. At ${jsonLoc.printLoc}")

          case o: Object[schema, props] =>
            json.asObject match
              case Some(obj) => decodeObject(o, jsonLoc, obj)
              case None => schemaViolation(s"Expected JSON object, got ${json.name}. At ${jsonLoc.printLoc}")

      case Schema.Unsupported(message) =>
        Result.Succeeded(Value.oops(message, details = Some(json.printWith(io.circe.Printer.spaces4))))
    }

  // Note: Ignores any superfluous fields, for better or worse.
  private def decodeObject[Props](
    schema: Schematic[Schema, Obj[Props]],
    jsonLoc: Stack[String],
    json: JsonObject,
  ): Result[Value[Obj[Props]]] =
    schema match
      case Object.Empty() =>
        Result.Succeeded(Value.obj)
      case Object.Snoc(init, pname, ptype) =>
        Result.map2(
          decodeObject(init, jsonLoc, json),
          decodeProp(pname, ptype, jsonLoc, json),
        ) { (init, last) =>
          Value.Object.extend(init, pname.value, last)
        }
      case Object.SnocOpt(init, pname, ptype) =>
        Result.map2(
          decodeObject(init, jsonLoc, json),
          decodePropOpt(pname, ptype, jsonLoc, json),
        ) { (init, last) =>
          Value.Object.extendOpt(init, pname.value, last)
        }

  private def decodeProp[K <: String, V](
    propName: SingletonType[K],
    propSchema: Schema[V],
    jsonLoc: Stack[String],
    jsonObject: JsonObject,
  ): Result[Value[V]] =
    jsonObject(propName.value) match
      case Some(json) =>
        val n = jsonLoc.size
        jsonLoc.push(".", propName.value)
        try {
          decodeAt(propSchema, jsonLoc, json)
        } finally {
          jsonLoc.dropInPlace(2)
        }
      case None =>
        schemaViolation(s"Missing property ${propName.value}. At ${jsonLoc.printLoc}")

  private def decodePropOpt[K <: String, V](
    propName: SingletonType[K],
    propSchema: Schema[V],
    jsonLoc: Stack[String],
    jsonObject: JsonObject,
  ): Result[Option[Value[V]]] =
    jsonObject(propName.value) match
      case Some(json) =>
        val n = jsonLoc.size
        jsonLoc.push(".", propName.value)
        try {
          decodeAt(propSchema, jsonLoc, json)
            .map(Some(_))
        } finally {
          jsonLoc.dropInPlace(2)
        }
      case None =>
        Result.Succeeded(None)

  private def decodeArray[T](
    elemSchema: Schema[T],
    jsonLoc: Stack[String],
    jsonElems: Vector[Json],
  ): Result[Value[Arr[T]]] = {
    val builder = IArray.newBuilder[Value[T]]
    builder.sizeHint(jsonElems.size)
    boundary {
      var i = 0
      for (json <- jsonElems) {
        jsonLoc.push(s"[$i]")
        try {
          decodeAt(elemSchema, jsonLoc, json) match
            case Result.Succeeded(t) => builder += t
            case Result.Failed(e) => boundary.break(Result.Failed(e))
        } finally {
          jsonLoc.pop()
        }
      }
      Result.Succeeded(Value.Array(builder.result))
    }
  }

  extension (jsonLoc: Stack[String]) {
    private def printLoc: String =
      jsonLoc.reverseIterator.mkString
  }
}
