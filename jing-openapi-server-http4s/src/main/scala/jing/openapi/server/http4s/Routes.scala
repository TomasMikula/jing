package jing.openapi.server.http4s

import cats.Functor
import org.http4s.{HttpApp, HttpRoutes}

/** Simple wrapper around [[org.http4s.HttpRoutes]] to avoid throwing `Kleisli`s at users. */
case class Routes[F[_]](http4sRoutes: HttpRoutes[F]) {
  def http4sApp(using Functor[F]): HttpApp[F] =
    http4sRoutes.orNotFound
}
