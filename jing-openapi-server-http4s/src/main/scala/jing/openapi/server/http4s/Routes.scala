package jing.openapi.server.http4s

import cats.data.OptionT
import cats.syntax.all.*
import cats.{Functor, Monad, MonadError}
import org.http4s
import org.http4s.{HttpApp, HttpRoutes}

/** Simple wrapper around [[org.http4s.HttpRoutes]] to avoid throwing `Kleisli`s at users. */
case class Routes[F[_]](http4sRoutes: HttpRoutes[F]) {
  def http4sApp(using Functor[F]): HttpApp[F] =
    http4sRoutes.orNotFound

  def foreachRequestResponse[A](
    f: http4s.Request[F] => F[A],
    g: (A, Option[http4s.Response[F]]) => F[Unit],
  )(using Monad[F]): Routes[F] =
    Routes:
      HttpRoutes: req =>
        OptionT:
          for
            a <- f(req)
            respOpt <- http4sRoutes(req).value
            _ <- g(a, respOpt)
          yield
            respOpt

  def foreachRequestResponseAttempt[A](
    f: http4s.Request[F] => F[A],
    g: (A, Either[Throwable, Option[http4s.Response[F]]]) => F[Unit],
  )(using MonadError[F, Throwable]): Routes[F] =
    Routes:
      HttpRoutes: req =>
        OptionT:
          for
            a <- f(req)
            respOpt <- http4sRoutes(req).value.attemptTap(g(a, _))
          yield
            respOpt
}
