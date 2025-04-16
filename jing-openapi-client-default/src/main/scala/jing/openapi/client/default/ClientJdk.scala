package jing.openapi.client.default

import java.io.IOException
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpRequest.{BodyPublisher, BodyPublishers}
import java.net.http.HttpResponse.BodyHandlers

import scala.jdk.OptionConverters.*

import io.circe.{Json, ParsingFailure}
import jing.openapi.model.{BodySchema, Client, DiscriminatedUnion, IsCaseOf, HttpThunk, RequestInput, ResponseSchema, Schema, Value, ValueMotif}
import libretto.lambda.util.Exists.Indeed

class ClientJdk extends Client {

  override type Response[T] = Result[Value.Lenient[T]]

  private type SupportedMimeType = "application/json"

  private val client =
    // TODO: manage as a resource, i.e. needs to be released
    HttpClient.newBuilder().build()

  override def runRequest[O](
    baseUrl: String,
    req: HttpThunk[O],
  ): Response[O] = {
    val HttpThunk.Impl(path, method, input, respSchema) = req

    val queryParams =
      input
        .queryParams
        .map { _.iterator.map { case (k, v) => s"${urlEncode(k)}=${urlEncode(v)}" }.mkString("?", "&", "") }
        .getOrElse("")

    val uri = new java.net.URI(baseUrl + path + queryParams)

    val (mimeTypeOpt, bodyPublisher) =
      encodeBody(input) match
        case Some((mimeType, bodyStr)) =>
          (Some(mimeType), BodyPublishers.ofString(bodyStr))
        case None =>
          (None, BodyPublishers.noBody())

    val resp =
      try {
        Result.Succeeded(
          client
            .send(
              HttpRequest
                .newBuilder(uri)
                .|>(mimeTypeOpt match
                  case Some(mt) => _.header("Content-Type", mt)
                  case None     => identity
                )
                .method(method.nameUpperCase, bodyPublisher)
                .build(),
              BodyHandlers.ofString(),
            )
        )
      } catch {
        case e: IOException => Result.ioError(e)
        case e              => Result.unexpectedError(e)
      }

    resp.flatMap(parseResponse(respSchema, _))
  }

  private def encodeBody[T](input: RequestInput[T]): Option[(SupportedMimeType, String)] =
    import RequestInput.*

    input match
      case NoInput                                    => None
      case Params(value)                              => None
      case Body(schema, value)                        => Some(encodeBody(schema, value))
      case ParamsAndBody(params, Body(schema, value)) => Some(encodeBody(schema, value))

  private def encodeBody[T](schema: BodySchema.NonEmpty[T], value: Value[T]): (SupportedMimeType, String) =
    schema match
      case schemaVariants: BodySchema.Variants[cases] =>
        (value: Value[DiscriminatedUnion[cases]]).handleDiscriminatedUnion(
          [Label <: String, A] => (
            discriminator: (Label IsCaseOf cases) { type Type = A },
            value: Value[A],
          ) =>
            val schema = schemaVariants.byMediaType.get(IsCaseOf.toMember(discriminator))
            encodeBody(schema, discriminator.label, value)
        )

  private def encodeBody[T](schema: Schema[T], mimeType: String, value: Value[T]): (SupportedMimeType, String) =
    mimeType match
      case appJson: "application/json" =>
        val bodyStr = ValueCodecJson.encode(schema, value)
        (appJson, bodyStr)
      case other =>
        // TODO: make this illegal state unrepresentable
        throw UnsupportedOperationException(s"MIME type '$other' not supported by ${this.getClass.getTypeName()}")

  private def parseResponse[T](
    schema: ResponseSchema[T],
    response: HttpResponse[String],
  ): Result[Value.Lenient[T]] = {
    val code = response.statusCode()
    schema.match
      case ResponseSchema.ByStatusCode(items) =>
        items.getOption(code.toString) match
          case Some(Indeed((i, s))) =>
            parseBody(code, s, response)
              .map(Value.Lenient.discriminatedUnion(IsCaseOf.fromMember(i), _))
          case None =>
            Result.unexpectedStatusCode(code, response.body())
  }

  private def parseBody[T](
    statusCode: Int,
    schema: BodySchema[T],
    response: HttpResponse[String],
  ): Result[Value.Lenient[T]] =
    schema match
      case BodySchema.Empty =>
        Result.Succeeded(Value.Lenient.unit)
      case BodySchema.Variants(byMediaType) =>
        response.headers().firstValue("Content-Type").toScala match
          case Some(contentType) =>
            byMediaType.getOption(contentType) match
              case Some(Indeed((i, s))) =>
                parseBody(statusCode, s, contentType, response.body())
                  .map(Value.Lenient.discriminatedUnion(IsCaseOf.fromMember(i), _))
              case None =>
                Result.unexpectedContentType(statusCode, contentType, response.body())
          case None =>
            Result.missingContentTypeHeader(statusCode, response.body())

  private def parseBody[T](
    statusCode: Int,
    schema: Schema[T],
    contentType: String,
    body: String,
  ): Result[Value.Lenient[T]] =
    contentType match
      case "application/json" =>
        io.circe.parser.parse(body) match
          case Left(failure) =>
            jsonParsingFailure(statusCode, failure)
          case Right(value) =>
            parseJsonBody(schema, value)
      case other =>
        Result.unsupportedContentType(statusCode, contentType, body)

  private def jsonParsingFailure[T](code: Int, e: ParsingFailure): Result[T] =
    Result.parseError(code, s"JSON parsing failure: ${e.message}", e.underlying)

  private def parseJsonBody[T](schema: Schema[T], body: Json): Result[Value.Lenient[T]] =
    ValueCodecJson.decodeLenient(schema, body)

  private def urlEncode(s: String): String =
    s // TODO

  private def urlEncode[T](v: Value[T]): String =
    urlEncode(stringify(v))

  private def stringify[T](v: Value[T]): String =
    import ValueMotif.*
    v.underlying match
      case StringValue(s) => s
      case Int64Value(i) => i.toString
      case Int32Value(i) => i.toString
      case BoolValue(b)  => b.toString
      case Array(elem) =>
        throw IllegalArgumentException("URL-encoding an array is not supported")
      case DiscUnion(_) =>
        throw IllegalArgumentException("URL-encoding a discriminated union is not supported")
      case Uno =>
        throw IllegalArgumentException("URL-encoding Unit is not supported")
      case o: Object[Value, props] =>
        // TODO: objects in query parameters should either
        //  - be disallowed by construction; or
        //  - have an associated encoder
        // What follows is wrong

        val builder = new StringBuilder
        builder += '{'

        var nonEmpty = false
        o.foreachProperty:
          [K <: String, V] => (k, v) =>
            if (nonEmpty) builder += ','
            builder ++= k
            builder += ':'
            builder ++= stringify(v)
            nonEmpty = true

        builder += '}'
        builder.result()

  extension [A](a: A)
    private def |>[B](f: A => B): B = f(a)
}
