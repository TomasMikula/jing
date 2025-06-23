package jing.openapi.server.http4s

import fs2.Stream
import fs2.text.utf8
import jing.openapi.model.{::, Body, DiscriminatedUnion, DiscriminatorOf, IsCaseOf, Value, ||}
import libretto.lambda.Items1Named
import org.http4s
import org.http4s.headers.*
import org.http4s.{Charset, Headers, MediaType}

sealed trait Response[+MimeType, +F[_], O]

object Response {
  case class Protocolary[MimeType, ByStatusCode](
    statusAndBody: Items1Named.Sum[||, ::, Body[MimeType, _], ByStatusCode],
  ) extends Response[MimeType, Nothing, ByStatusCode]

  case class Custom[F[_], O](value: http4s.Response[F]) extends Response[Nothing, F, O]

  class Builder[ResponseType, StatusCode <: DiscriminatorOf[ResponseType], SupportedMimeType] {
    def status(code: StatusCode & String)(using
      c: code.type IsCaseOf ResponseType,
    ): Builder.WithStatus[code.type, c.Type, ResponseType, SupportedMimeType] =
      Builder.WithStatus(c)
  }

  object Builder {
    case class WithStatus[S <: String, BodyType, ResponseType, SupportedMimeType](
      i: (S IsCaseOf ResponseType) { type Type = BodyType },
    )

    extension [S <: String, BodyTypesByMimeType, ResponseType, SupportedMimeType](
      builder: WithStatus[S, DiscriminatedUnion[BodyTypesByMimeType], ResponseType, SupportedMimeType]
    ) {
      def body: PendingMimeType[S, BodyTypesByMimeType, ResponseType, SupportedMimeType] =
        PendingMimeType(builder.i)
    }

    class PendingMimeType[S <: String, BodyTypesByMimeType, ResponseType, SupportedMimeType](
      i: (S IsCaseOf ResponseType) { type Type = DiscriminatedUnion[BodyTypesByMimeType] }
    ) {
      def apply[MT <: SupportedMimeType](using
        j: MT IsCaseOf BodyTypesByMimeType,
      ): PendingBody[S, MT, j.Type, BodyTypesByMimeType, ResponseType] =
        PendingBody(i, j)
    }

    class PendingBody[S <: String, MT, BodyType, BodyTypesByMimeType, ResponseType](
      i: (S IsCaseOf ResponseType) { type Type = DiscriminatedUnion[BodyTypesByMimeType] },
      j: (MT IsCaseOf BodyTypesByMimeType) { type Type = BodyType },
    ) {
      def apply(
        body: Value[BodyType],
      ): Response.Protocolary[MT, ResponseType] =
        Response.Protocolary(
          Items1Named.Sum.Value[||, ::, Body[MT, _], S, i.Type, ResponseType](
            tag = IsCaseOf.toMember(i),
            value = Body.MimeVariant[MT, j.Type, BodyTypesByMimeType](j, body)
          )
        )
    }
  }

  def apply[SupportedMimeType, O](
    f: Builder[O, DiscriminatorOf[O], SupportedMimeType] => Response.Protocolary[SupportedMimeType, O],
  ): Response[SupportedMimeType, Nothing, O] =
    f(new Builder)

  /** Creates a custom (i.e. circumventing the schema) application/json response. */
  def json[O](
    status: http4s.Status,
    jsonBody: String,
  ): Response.Custom[fs2.Pure, O] =
    Custom(
      http4s.Response(
        status = status,
        headers = Headers(
          `Content-Type`(MediaType.application.json, Charset.`UTF-8`),
        ),
        body = fs2.Stream(jsonBody).through(utf8.encode),
      )
    )

  /** Creates a custom (i.e. circumventing the schema) text/plain response. */
  def plainText[O](
    status: http4s.Status,
    body: String = "",
  ): Response.Custom[fs2.Pure, O] =
    Custom(
      http4s.Response(
        status = status,
        headers = Headers(
          `Content-Type`(MediaType.text.plain, Charset.`UTF-8`),
        ),
        body = fs2.Stream(body).through(utf8.encode),
      )
    )
}
