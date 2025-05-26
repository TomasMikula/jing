package jing.openapi.server.http4s

import cats.Monad
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import java.nio.charset.StandardCharsets.UTF_8
import jing.openapi.model.{HttpEndpoint, Value}
import jing.openapi.model.server.ServerBuilder
import jing.openapi.model.server.ServerBuilder.EndpointHandler
import org.http4s.{Headers, HttpRoutes, Request, Response, Status}

class Http4sServerBuilder[F[_]](using
  Monad[F],
) extends ServerBuilder {
  import Http4sServerBuilder.*

  override type RequestHandler[I, O] =
    // TODO: dedicated ADT for RequestHandler, to support multiple forms of handlers
    Value[I] => F[Value[O]]

  override type ServerDefinition =
    HttpRoutes[F]

  override def build(endpointHandlers: List[EndpointHandler[RequestHandler]]): HttpRoutes[F] =
    endpointHandlers.foldRight(HttpRoutes.empty[F]) { (h, fallback) =>
      endpointRoute(h).orElse(fallback)
    }

  private def endpointRoute(h: EndpointHandler[RequestHandler]): HttpRoutes[F] =
    HttpRoutes { req =>
      parseRequest(h.endpoint, req) match
        case None =>
          OptionT.none
        case Some(Left(BadRequest(detail))) =>
          // TODO: best effort to respect Accept request header
          OptionT.some(Response(
            Status.BadRequest,
            headers = Headers("Content-Type" -> "text/plain"),
            body = Stream.chunk(Chunk.byteBuffer(UTF_8.encode(detail))),
          ))
        case Some(Right(inValue)) =>
          OptionT.liftF(
            h.requestHandler(inValue)
              .map(valueToResponse(h.endpoint, _))
          )
    }

  private def parseRequest[I](
    ep: HttpEndpoint[I, ?],
    req: Request[F],
  ): Option[Either[BadRequest, Value[I]]] =
    ???

  private def valueToResponse[O](
    ep: HttpEndpoint[?, O],
    value: Value[O],
  ): Response[F] =
    ???
}

object Http4sServerBuilder {
  def forIO: Http4sServerBuilder[IO] =
    forF[IO]

  def forF[F[_]: Monad]: Http4sServerBuilder[F] =
    Http4sServerBuilder[F]

  private[Http4sServerBuilder] case class BadRequest(detail: String)
}
