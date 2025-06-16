package jing.openapi.server.http4s

import fs2.Stream
import fs2.text.utf8
import jing.openapi.model.Value
import org.http4s
import org.http4s.headers.*
import org.http4s.{Charset, Headers, MediaType}

sealed trait Response[+F[_], O]

object Response {
  case class Protocolary[O](value: Value[O]) extends Response[Nothing, O]

  case class Custom[F[_], O](value: http4s.Response[F]) extends Response[F, O]

  def apply[O](value: Value[O]): Response[Nothing, O] =
    Protocolary(value)

  /** Creates a custom (i.e. circumventing the schema) application/json response. */
  def json[O](
    status: http4s.Status,
    jsonBody: String,
  ): Response[fs2.Pure, O] =
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
  ): Response[fs2.Pure, O] =
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
