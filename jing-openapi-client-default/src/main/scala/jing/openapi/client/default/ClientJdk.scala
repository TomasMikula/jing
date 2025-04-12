package jing.openapi.client.default

import java.io.IOException
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpRequest.{BodyPublisher, BodyPublishers}
import java.net.http.HttpResponse.BodyHandlers

import scala.jdk.OptionConverters.*

import io.circe.{Json, ParsingFailure}
import jing.openapi.model.{BodySchema, Client, DiscriminatedUnion, IsCaseOf, HttpThunk, RequestInput, ResponseSchema, Schema, Value}
import libretto.lambda.util.Exists.Indeed

class ClientJdk extends Client {

  override type Response[T] = Result[Value[T]]

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

  private def encodeBody[T](schema: BodySchema[T], value: Value[T]): (SupportedMimeType, String) =
    schema match
      case BodySchema.EmptyBody => throw IllegalArgumentException("Empty body schema") // TODO: make unrepresentable
      case schemaVariants: BodySchema.Variants[cases] =>
        val duValue =
          Value.asDiscriminatedUnion(value: Value[DiscriminatedUnion[cases]])
        val schema = schemaVariants.byMediaType.get(IsCaseOf.toMember(duValue.discriminator))
        encodeBody(schema, duValue.discriminatorValue, duValue.value)

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
  ): Result[Value[T]] = {
    val code = response.statusCode()
    schema.match
      case ResponseSchema.ByStatusCode(items) =>
        items.getOption(code.toString) match
          case Some(Indeed((i, s))) =>
            parseBody(code, s, response)
              .map(Value.discriminatedUnion(i, _))
          case None =>
            Result.unexpectedStatusCode(code, response.body())
  }

  private def parseBody[T](
    statusCode: Int,
    schema: BodySchema[T],
    response: HttpResponse[String],
  ): Result[Value[T]] =
    schema match
      case BodySchema.EmptyBody =>
        Result.Succeeded(Value.unit)
      case BodySchema.Variants(byMediaType) =>
        response.headers().firstValue("Content-Type").toScala match
          case Some(contentType) =>
            byMediaType.getOption(contentType) match
              case Some(Indeed((i, s))) =>
                parseBody(statusCode, s, contentType, response.body())
                  .map(Value.discriminatedUnion(i, _))
              case None =>
                Result.unexpectedContentType(statusCode, contentType, response.body())
          case None =>
            Result.missingContentTypeHeader(statusCode, response.body())

  private def parseBody[T](
    statusCode: Int,
    schema: Schema[T],
    contentType: String,
    body: String,
  ): Result[Value[T]] =
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

  private def parseJsonBody[T](schema: Schema[T], body: Json): Result[Value[T]] =
    ValueCodecJson.decode(schema, body)

  private def urlEncode(s: String): String =
    s // TODO

  private def urlEncode[T](v: Value[T]): String =
    urlEncode(stringify(v))

  private def stringify[T](v: Value[T]): String =
    import Value.*
    v match
        case StringValue(s) => s
        case Int64Value(i) => i.toString
        case Int32Value(i) => i.toString
        case BoolValue(b)  => b.toString
        case o: Object[props] =>
          // TODO: objects in query parameters should either
          //  - be disallowed by construction; or
          //  - have an associated encoder
          Value.toMap(o)
            .view.mapValues(stringify(_))
            .iterator
            .map { case (k, v) => s"$k:$v" }
            .mkString("{", ",", "}")

  extension [A](a: A)
    private def |>[B](f: A => B): B = f(a)
}
