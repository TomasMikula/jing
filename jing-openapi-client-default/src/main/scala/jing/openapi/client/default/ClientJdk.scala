package jing.openapi.client.default

import java.io.IOException
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.http.HttpRequest.{BodyPublisher, BodyPublishers}
import java.net.http.HttpResponse.BodyHandlers

import scala.jdk.OptionConverters.*

import io.circe.{Json, ParsingFailure}
import jing.openapi.model.{||, ::, :?, Body, BodySchema, IsCaseOf, Obj, RequestSchema, ResponseSchema, Schema, Value, ValueMotif}
import jing.openapi.model.client.{Client, HttpThunk}
import libretto.lambda.util.Exists.Indeed

class ClientJdk extends Client {

  override type Response[T] = Result[Value.Lenient[T]]

  override type SupportedMimeType = "application/json"

  private val client =
    // TODO: manage as a resource, i.e. needs to be released
    HttpClient.newBuilder().build()

  override def runRequest[O](
    baseUrl: String,
    req: HttpThunk[SupportedMimeType, O],
  ): Response[O] =
    req match {
      case HttpThunk.Impl(method, paramsSchema, params, body, respSchema) =>
        val relativeUrl = toRelativeUrl(paramsSchema, params)
        val uri = new java.net.URI(baseUrl + relativeUrl)

        val (mimeTypeOpt, bodyPublisher) =
          body.map(encodeBody) match
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

  def toRelativeUrl[Ps](
    paramsSchema: RequestSchema.Params[Ps],
    params: Value[Obj[Ps]],
  ): String =
    toRelativeUrl_(paramsSchema, params)._1

  def toRelativeUrl_[Ps](
    paramsSchema: RequestSchema.Params[Ps],
    params: Value[Obj[Ps]],
  ): (String, Boolean) = {
    import RequestSchema.Params.*

    paramsSchema match
      case ConstantPath(path) =>
        (path, false)
      case ParameterizedPath(path) =>
        (toRelativeUrl(path, params), false)
      case wqp: WithQueryParam[init, pName, pType] =>
        val (initParams, lastParam) =  (params: Value[Obj[init || pName :: pType]]).unsnoc
        val (initUrl, hasQueryParams) = toRelativeUrl_(wqp.init, initParams)
        val url =
          initUrl
            + (if hasQueryParams then "&" else "?")
            + encodeQueryParam(wqp.pName.value, lastParam)
        (url, true)
      case wqpo: WithQueryParamOpt[init, pName, pType] =>
        val (initParams, lastParam) =  (params: Value[Obj[init || pName :? pType]]).unsnoc
        val (initUrl, hasQueryParams) = toRelativeUrl_(wqpo.init, initParams)
        lastParam match
          case None =>
            (initUrl, hasQueryParams)
          case Some(lastParam) =>
            val url =
              initUrl
                + (if hasQueryParams then "&" else "?")
                + encodeQueryParam(wqpo.pName.value, lastParam)
            (url, true)
  }

  private def toRelativeUrl[Ps](
    path: RequestSchema.Path[Ps],
    params: Value[Obj[Ps]],
  ): String = {
    import RequestSchema.Path.*

    path match
      case Constant(value) =>
        urlEncode(value)
      case wp: WithParam[init, pname, ptype] =>
        val WithParam(init, pName, pSchema, suffix) = wp
        val (initParams, lastParam) = (params: Value[Obj[init || pname :: ptype]]).unsnoc
        toRelativeUrl(init, initParams)
          + urlEncode(lastParam)
          + urlEncode(suffix)
  }

  private def encodeBody[T](body: Body[SupportedMimeType, T]): (SupportedMimeType, String) =
    body match
      case Body.MimeVariant(schemaVariants, i, v) =>
        schemaVariants match
          case schemaVariants: BodySchema.Variants[cases] =>
            val schema = schemaVariants.byMediaType.get(IsCaseOf.toMember(i))
            (i.label, encodeBody(schema, i.label, v))

  private def encodeBody[T](schema: Schema[T], mimeType: SupportedMimeType, value: Value[T]): String =
    mimeType match
      case appJson: "application/json" =>
        ValueCodecJson.encode(schema, value)

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
      case BodySchema.AnythingAsPlainText =>
        Result.Succeeded(Value.Lenient.str(response.body()))

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

  private def encodeQueryParam[Q](k: String, v: Value[Q]): String =
    s"${urlEncode(k)}=${urlEncode(v)}"

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
