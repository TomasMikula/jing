package jing.openapi.server.http4s

import cats.Monad
import cats.data.{EitherT, NonEmptyList, OptionT}
import cats.effect.IO
import cats.syntax.all.*
import fs2.{Chunk, RaiseThrowable, Stream}
import jing.openapi.model.server.ServerBuilder
import jing.openapi.model.server.ServerBuilder.EndpointHandler
import jing.openapi.model.{::, :?, BodySchema, DiscriminatedUnion, HttpEndpoint, Obj, RequestSchema, ResponseSchema, Value, ValueCodecJson, ||}
import libretto.lambda.Items1Named
import org.http4s
import org.http4s.{Headers, HttpRoutes, MediaType, Request, Status, Uri}

import java.nio.charset.StandardCharsets.UTF_8

class Http4sServerBuilder[F[_]](using
  Monad[F],
  RaiseThrowable[F],
) extends ServerBuilder {
  import Http4sServerBuilder.*

  override type RequestHandler[I, O] =
    // TODO: introduce dedicated ADT for RequestHandler, to support multiple forms of handlers
    Value[Obj[I]] => F[Response[F, O]]

  override type ServerDefinition =
    Routes[F]

  override def build(endpointHandlers: List[EndpointHandler[RequestHandler]]): Routes[F] =
    Routes(
      endpointHandlers.foldRight(HttpRoutes.empty[F]) { (h, fallback) =>
        endpointRoute(h).orElse(fallback)
      }
    )

  private def endpointRoute(h: EndpointHandler[RequestHandler]): HttpRoutes[F] =
    HttpRoutes { req =>
      parseRequest(h.endpoint, req) match
        case None =>
          OptionT.none
        case Some(Left(error)) =>
          // TODO: best effort to respect Accept request header
          error match
            case BadRequest(detail) =>
              OptionT.some(http4s.Response(
                Status.BadRequest,
                headers = Headers("Content-Type" -> "text/plain"),
                body = Stream.chunk(Chunk.byteBuffer(UTF_8.encode(detail))),
              ))
            case UnsupportedContentType(mediaType) =>
              // TODO: return BadRequest if the given media type is not permitted by the spec
              OptionT.some(http4s.Response(
                Status.NotImplemented,
                headers = Headers("Content-Type" -> "text/plain"),
                body = Stream.chunk(Chunk.byteBuffer(UTF_8.encode(s"Unsupported Content-Type ${mediaType.toString}. Expected application/json"))),
              ))

        case Some(Right(inValue)) =>
          OptionT.liftF(
            h.requestHandler(inValue)
              .map(encodeResponse(h.endpoint, _))
          )
    }
}

object Http4sServerBuilder {
  def forIO: Http4sServerBuilder[IO] =
    forF[IO]

  def forF[F[_]: Monad: RaiseThrowable]: Http4sServerBuilder[F] =
    Http4sServerBuilder[F]

  private sealed trait ParseError
  private case class BadRequest(detail: String) extends ParseError
  private case class UnsupportedContentType(contentType: MediaType) extends ParseError

  private def parseRequest[I, F[_]](
    ep: HttpEndpoint[I, ?],
    req: Request[F],
  )(using
    RaiseThrowable[F],
  ): Option[Either[ParseError, Value[Obj[I]]]] = { // TODO: consider EitherT[Option, ...]
    import RequestSchema.{ConstantPath, Parameterized, WithBody}

    if (ep.method.nameUpperCase != req.method.name)
      None
    else
      ep.requestSchema match
        case ConstantPath(path) =>
          summon[I =:= Void]
          if (path != req.uri.path.renderString)
            None
          else
            Some(Right(Value.obj))
        case Parameterized(params) =>
          route(params, req.uri)
            .map(_.map(ps => Value.obj.set("params", ps)))
        case WithBody(params, body) =>
          def parseBody =
            req.contentType match
              case Some(ct) if !ct.mediaType.satisfies(MediaType("application", "json")) =>
                Left(UnsupportedContentType(ct.mediaType))
              case Some(_) | None =>
                parseAsJson(body, req.bodyText)
          params match
            case ConstantPath(path) =>
              if (path != req.uri.path.renderString)
                None
              else
                parseBody
                  .map { b => Value.obj.set("body", b) }
                  .some
            case Parameterized(params) =>
              route(params, req.uri)
                .map {
                  _.flatMap { ps =>
                    parseBody
                      .map { b => Value.obj.set("params", ps).set("body", b) }
                  }
                }
  }

  private def route[Ps](
    paramSchema: RequestSchema.Params.Proper[Ps],
    uri: Uri,
  ): Option[Either[Nothing, Value[Obj[Ps]]]] = {
    val Uri(_, _, path, query, _) = uri
    route(paramSchema, path.renderString, query.multiParams).value match
      case None =>
        None
      case Some(Left(e)) =>
        Some(Left(e))
      case Some(Right((result = res, leftovers = remaining))) =>
        remaining match
          case (Some(path), _) =>
            // treat unconsumed path as a non-match
            None
          case (None, query) =>
            if (query.isEmpty)
              Some(Right(res))
            else
              // treat extraneous query params as bad request
              ???
  }

  private def route[Ps](
    paramSchema: RequestSchema.Params[Ps],
    path: String,
    query: Map[String, Seq[String]],
  ): EitherT[
    Option,
    Nothing,
    ( result: Value[Obj[Ps]]
    , leftovers: (path: Option[String], query: Map[String, Seq[String]])
    ),
  ] = {
    import RequestSchema.Params.*

    paramSchema match
      case ConstantPath(p) =>
        summon[Ps =:= Void]
        if (path.startsWith(p))
          val leftoverPath = if path.length == p.length then None else Some(path.drop(p.length))
          EitherT.pure((result = Value.obj, leftovers = (leftoverPath, query)))
        else
          EitherT.liftF(None)
      case ParameterizedPath(p) =>
        route(p, path)
          .map { case (res, leftoverPath) => (res, (leftoverPath, query)) }
      case WithQueryParam(init, pName, pSchema) =>
        ???
      case ps: WithQueryParamOpt[init, k, v] =>
        route(ps.init, path, query).flatMap { case (result = initRes, leftovers = remaining) =>
          val ev = summon[Ps =:= (init || k :? v)]
          remaining match
            case (None, q) =>
              q.get(ps.pName.value) match
                case None =>
                  val res: Value[Obj[Ps]] =
                    ev.substituteContra[[x] =>> Value[Obj[x]]]:
                      initRes.extendOpt(ps.pName, None)
                  EitherT.pure((result = res, leftovers = (None, q)))
                case Some(values) =>
                  ???
            case (Some(p), _) =>
              // leftover path when there's no more path to match => return no match
              EitherT.liftF(None)
        }
  }

  private def route[Ps](
    pathSchema: RequestSchema.Path[Ps],
    path: String,
  ): EitherT[
    Option,
    Nothing,
    ( result: Value[Obj[Ps]]
    , leftoverPath: Option[String]
    ),
  ] = {
    import RequestSchema.Path.{Constant, WithParam}

    pathSchema match
      case Constant(p) =>
        if (path.startsWith(p))
          val leftoverPath = if path.length == p.length then None else Some(path.drop(p.length))
          EitherT.pure((result = Value.obj, leftoverPath = leftoverPath))
        else
          EitherT.liftF(None)
      case WithParam(prefix, pName, pSchema, suffix) =>
        route(prefix, path).flatMap { case (prefixRes, leftoverOpt) =>
          leftoverOpt match
            case None =>
              EitherT.liftF(None)
            case Some(leftover) =>
              EitherT
                .fromEither(parseParam(pSchema, leftover))
                .flatMap { case (pValue, leftoverOpt) =>
                  ???
                }
        }
  }

  private def parseParam[T](
    pSchema: RequestSchema.Path.ParamSchema[T],
    path: String,
  ): Either[Nothing, (result: Value[T], leftoverPath: Option[String])] =
    ???

  private def parseAsJson[B, F[_]](
    schema: BodySchema.NonEmpty[B],
    req: Stream[F, String],
  ): Either[BadRequest, Value[B]] =
    ???

  private def encodeResponse[O, F[_]](
    ep: HttpEndpoint[?, O],
    resp: Response[F, O],
  ): http4s.Response[F] =
    resp match
      case Response.Protocolary(value) =>
        ep.responseSchema match
          case ResponseSchema.ByStatusCode(schemasByStatusCode) =>
            encodeResponse(schemasByStatusCode, value)
      case Response.Custom(r) =>
        r.covary

  private def encodeResponse[Cases, F[_]](
    schemasByStatusCode: Items1Named.Product[||, ::, BodySchema, Cases],
    value: Value[DiscriminatedUnion[Cases]],
  ): http4s.Response[F] =
    ???
}
