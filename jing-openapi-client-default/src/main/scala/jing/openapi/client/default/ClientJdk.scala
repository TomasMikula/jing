package jing.openapi.client.default

import java.io.IOException
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpResponse.BodyHandlers

import scala.jdk.OptionConverters.*

import io.circe.{Json, ParsingFailure}
import jing.openapi.model.{BodySchema, Client, HttpThunk, ResponseSchema, Schema, Value}
import libretto.lambda.util.Exists.Indeed

class ClientJdk extends Client {

  override type Response[T] = Result[Value[T]]

  private val client =
    // TODO: manage as a resource, i.e. needs to be released
    HttpClient.newBuilder().build()

  override def runRequest[O](
    baseUrl: String,
    req: HttpThunk[O],
  ): Response[O] = {
    val HttpThunk.Impl(path, input, respSchema) = req

    val queryParams =
      input
        .queryParams
        .map { _.iterator.map { case (k, v) => s"${urlEncode(k)}=${urlEncode(v)}" }.mkString("?", "&", "") }
        .getOrElse("")

    val uri = new java.net.URI(baseUrl + path + queryParams)

    val resp =
      try {
        Result.Success(
          client
            .send(
              HttpRequest.newBuilder(uri).build(), // TODO: body
              BodyHandlers.ofString(), // XXX
            )
        )
      } catch {
        case e: IOException => Result.Failure.IOError(e)
        case e              => Result.Failure.UnexpectedError(e)
      }

    resp.flatMap(parseResponse(respSchema, _))
  }

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
            Result.Failure.UnexpectedStatusCode(code, response.body())
  }

  private def parseBody[T](
    statusCode: Int,
    schema: BodySchema[T],
    response: HttpResponse[String],
  ): Result[Value[T]] =
    schema match
      case BodySchema.EmptyBody =>
        Result.Success(Value.unit)
      case BodySchema.Variants(byMediaType) =>
        response.headers().firstValue("Content-Type").toScala match
          case Some(contentType) =>
            byMediaType.getOption(contentType) match
              case Some(Indeed((i, s))) =>
                parseBody(statusCode, s, contentType, response.body())
                  .map(Value.discriminatedUnion(i, _))
              case None =>
                Result.Failure.UnexpectedContentType(statusCode, contentType, response.body())
          case None =>
            Result.Failure.MissingContentTypeHeader(statusCode, response.body())

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
        Result.Failure.UnsupportedContentType(statusCode, contentType, body)

  private def jsonParsingFailure(code: Int, e: ParsingFailure): Result[Nothing] =
    Result.Failure.ParseError(code, s"JSON parsing failure: ${e.message}", e.underlying)

  private def parseJsonBody[T](schema: Schema[T], body: Json): Result[Value[T]] =
    JsonToValue.decode(schema, body)

  private def urlEncode(s: String): String =
    s // TODO

  private def urlEncode[T](v: Value[T]): String =
    urlEncode(stringify(v))

  private def stringify[T](v: Value[T]): String =
    import Value.*
    v match
        case StringValue(s) => s
        case Int64Value(i) => i.toString
        case o: Object[props] =>
          // TODO: objects in query parameters should either
          //  - be disallowed by construction; or
          //  - have an associated encoder
          Value.toMap(o)
            .view.mapValues(stringify(_))
            .iterator
            .map { case (k, v) => s"$k:$v" }
            .mkString("{", ",", "}")

}
