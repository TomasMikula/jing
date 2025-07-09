package jing.openapi.model

import io.circe.{Json, JsonObject}
import jing.openapi.model.{||, ::, :?, Arr, Enum, Obj, Oops, ScalaUnionOf, ScalaValueOf, Schema, SchemaMotif, Value, ValueMotif}
import jing.openapi.model.SchemaMotif.{Array, Enumeration, I32, I64, S, B, Object}
import libretto.lambda.Items1
import libretto.lambda.util.SingletonType
import scala.util.boundary
import scala.collection.mutable.Stack

object ValueCodecJson {
  enum DecodeResult[T]:
    case Succeeded(value: T)
    case SchemaViolation(details: String)

    def map[U](f: T => U): DecodeResult[U] =
      this match
        case Succeeded(value) => Succeeded(f(value))
        case SchemaViolation(details) => SchemaViolation(details)

    def flatMap[U](f: T => DecodeResult[U]): DecodeResult[U] =
      this match
        case Succeeded(value) => f(value)
        case SchemaViolation(details) => SchemaViolation(details)

  object DecodeResult:
    // TODO: accumulate errors (but first need to be able to represent multiple errors)
    def map2[A, B, R](a: DecodeResult[A], b: DecodeResult[B])(f: (A, B) => R): DecodeResult[R] =
      (a, b) match
        case (Succeeded(a), Succeeded(b)) => Succeeded(f(a, b))
        case (SchemaViolation(details), _) => SchemaViolation(details)
        case (_, SchemaViolation(details)) => SchemaViolation(details)

  import DecodeResult.{SchemaViolation, Succeeded}

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
          case e: Enumeration[Schema, base, cases] =>
            summon[T =:= Enum[base, cases]]
            encode(Schema.Proper(e.baseType), Value.widenEnum(value: Value[Enum[base, cases]]), builder)
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
            encodeObjectProps(o.value, value, builder)
            builder += '}'

      case u: Schema.Unsupported[s] =>
        value.isNotOops[s]
    }

  private def encodeObjectProps[Props](
    schema: ObjectMotif[Schema, Schema, Props],
    value: Value[Obj[Props]],
    builder: StringBuilder,
  ): Boolean =
    schema match
      case ObjectMotif.Empty() =>
        false
      case s: ObjectMotif.Snoc[sch1, sch2, init, pname, ptype] =>
        summon[Props =:= (init || pname :: ptype)]
        val (vInit, vLast) = Value.unsnoc[init, pname, ptype](value)
        val propsWritten = encodeObjectProps(s.init, vInit, builder)
        appendProp(s.pval, s.pname.value, vLast, propsWritten, builder)
        true
      case s: ObjectMotif.SnocOpt[sch1, sch2, init, pname, ptype] =>
        summon[Props =:= (init || pname :? ptype)]
        val (vInit, vLastOpt) = Value.unsnoc[init, pname, ptype](value)
        val propsWritten = encodeObjectProps(s.init, vInit, builder)
        vLastOpt match
          case None =>
            propsWritten
          case Some(vLast) =>
            appendProp(s.pval, s.pname.value, vLast, propsWritten, builder)
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

  def decodeLenient[T](schema: Schema[T], json: Json): DecodeResult[Value.Lenient[T]] =
    val jsonLoc = Stack("<root>")
    decodeLenientAt(schema, jsonLoc, json)

  private def decodeLenientAt[T](
    schema: Schema[T],
    jsonLoc: Stack[String],
    json: Json,
  ): DecodeResult[Value.Lenient[T]] =
    schema match {
      case Schema.Proper(s) =>
        s match
          case I32() =>
            json.asNumber match
              case Some(jsonNumber) =>
                jsonNumber.toInt match
                  case Some(n) => Succeeded(Value.Lenient.int32(n))
                  case None => SchemaViolation(s"Expected a 32-bit integer, got ${jsonNumber}. At ${jsonLoc.printLoc}")
              case None =>
                SchemaViolation(s"Expected JSON number, got ${json.name} (${json.noSpaces}). At ${jsonLoc.printLoc}")

          case I64() =>
            json.asNumber match
              case Some(jsonNumber) =>
                jsonNumber.toLong match
                  case Some(n) => Succeeded(Value.Lenient.int64(n))
                  case None => SchemaViolation(s"Expected a 64-bit integer, got ${jsonNumber}. At ${jsonLoc.printLoc}")
              case None =>
                SchemaViolation(s"Expected JSON number, got ${json.name} (${json.noSpaces}). At ${jsonLoc.printLoc}")

          case S() =>
            json.asString match
              case Some(s) => Succeeded(Value.Lenient.str(s))
              case None => SchemaViolation(s"Expected JSON string, got ${json.name} (${json.noSpaces}). At ${jsonLoc.printLoc}")

          case B() =>
            json.asBoolean match
              case Some(b) => Succeeded(Value.Lenient.bool(b))
              case None => SchemaViolation(s"Expected JSON boolean, got ${json.name} (${json.noSpaces}). At ${jsonLoc.printLoc}")

          case Enumeration(base, cases) =>
            decodeLenientAt(Schema.Proper(base), jsonLoc, json)
              .flatMap {
                case p: Value.Lenient.Proper[t] =>
                  val Value.Lenient.Proper(v) = p
                  decodeEnumValueLenient(cases, v) match
                    case Some(v) =>
                      Succeeded(Value.Lenient.Proper(v))
                    case None =>
                      val expected = cases.toList([a] => a => a.show).mkString(", ")
                      val actual = Value.Lenient.Proper(v).show
                      SchemaViolation(s"Expected one of ${expected}, got ${actual}. At ${jsonLoc.printLoc}")
                case _: Value.Lenient.Oopsy[s] =>
                  base.isNotOops[s]
              }

          case Array(elemSchema) =>
            json.asArray match
              case Some(jsonElems) => decodeArrayLenient(elemSchema, jsonLoc, jsonElems)
              case None => SchemaViolation(s"Expected JSON array, got ${json.name} (${json.noSpaces}). At ${jsonLoc.printLoc}")

          case o: Object[schema, props] =>
            json.asObject match
              case Some(obj) => decodeObjectLenient(o.asObject.value, jsonLoc, obj)
              case None => SchemaViolation(s"Expected JSON object, got ${json.name} (${json.noSpaces}). At ${jsonLoc.printLoc}")

      case Schema.Unsupported(message) =>
        Succeeded(Value.Lenient.oops(message, details = Some(json.printWith(io.circe.Printer.spaces4))))
    }

  private def decodeEnumValueLenient[F[_], Base, Cases](
    cases: Items1.Product[||, Void, ScalaValueOf[_, Base], Cases],
    value: ValueMotif[F, Base],
  ): Option[ValueMotif[F, Enum[Base, Cases]]] =
    cases match
      case s: Items1.Product.Single[sep, void, svo, a] =>
        summon[Cases =:= (Void || a)]
        summon[a <:< ScalaUnionOf[Void || a]]
        if (value.sameAs(s.value))
          Some(ValueMotif.EnumValue[Base, Void || a, a](s.value))
        else
          None
      case s: Items1.Product.Snoc[sep, void, svo, init, last] =>
        summon[Cases =:= (init || last)]
        summon[last <:< ScalaUnionOf[init || last]]
        val Items1.Product.Snoc(init, last) = s
        if (value.sameAs(last))
          Some(ValueMotif.EnumValue[Base, init || last, last](last))
        else
          decodeEnumValueLenient[F, Base, init](init, value)
            .map(_.extendEnum[last])

  // Note: Ignores any superfluous fields, for better or worse.
  private def decodeObjectLenient[Props](
    schema: ObjectMotif[Schema, Schema, Props],
    jsonLoc: Stack[String],
    json: JsonObject,
  ): DecodeResult[Value.Lenient[Obj[Props]]] =
    schema match
      case ObjectMotif.Empty() =>
        Succeeded(Value.Lenient.obj.empty)
      case ObjectMotif.Snoc(init, pname, ptype) =>
        DecodeResult.map2(
          decodeObjectLenient(init, jsonLoc, json),
          decodePropLenient(pname, ptype, jsonLoc, json),
        ) { (init, last) =>
          init.extend(pname, last)
        }
      case ObjectMotif.SnocOpt(init, pname, ptype) =>
        DecodeResult.map2(
          decodeObjectLenient(init, jsonLoc, json),
          decodePropOptLenient(pname, ptype, jsonLoc, json),
        ) { (init, last) =>
          init.extendOpt(pname, last)
        }

  private def decodePropLenient[K <: String, V](
    propName: SingletonType[K],
    propSchema: Schema[V],
    jsonLoc: Stack[String],
    jsonObject: JsonObject,
  ): DecodeResult[Value.Lenient[V]] =
    jsonObject(propName.value) match
      case Some(json) =>
        val n = jsonLoc.size
        jsonLoc.push(".", propName.value)
        try {
          decodeLenientAt(propSchema, jsonLoc, json)
        } finally {
          jsonLoc.dropInPlace(2)
        }
      case None =>
        SchemaViolation(s"Missing property ${propName.value}. At ${jsonLoc.printLoc}")

  private def decodePropOptLenient[K <: String, V](
    propName: SingletonType[K],
    propSchema: Schema[V],
    jsonLoc: Stack[String],
    jsonObject: JsonObject,
  ): DecodeResult[Option[Value.Lenient[V]]] =
    jsonObject(propName.value) match
      case Some(json) =>
        val n = jsonLoc.size
        jsonLoc.push(".", propName.value)
        try {
          decodeLenientAt(propSchema, jsonLoc, json)
            .map(Some(_))
        } finally {
          jsonLoc.dropInPlace(2)
        }
      case None =>
        Succeeded(None)

  private def decodeArrayLenient[T](
    elemSchema: Schema[T],
    jsonLoc: Stack[String],
    jsonElems: Vector[Json],
  ): DecodeResult[Value.Lenient[Arr[T]]] = {
    val builder = IArray.newBuilder[Value.Lenient[T]]
    builder.sizeHint(jsonElems.size)
    boundary {
      var i = 0
      for (json <- jsonElems) {
        jsonLoc.push(s"[$i]")
        try {
          decodeLenientAt(elemSchema, jsonLoc, json) match
            case Succeeded(t) => builder += t
            case SchemaViolation(e) => boundary.break(SchemaViolation(e))
        } finally {
          jsonLoc.pop()
          i += 1
        }
      }
      Succeeded(Value.Lenient.arr(builder.result))
    }
  }

  extension (jsonLoc: Stack[String]) {
    private def printLoc: String =
      jsonLoc.reverseIterator.mkString
  }
}
