package jing.openapi

import jing.openapi.model.Schema
import libretto.lambda.util.{Applicative, Exists}
import libretto.lambda.util.Exists.Indeed
import scala.quoted.*

trait SchemaLookup[F[_]] {
  def lookup(schemaName: String): Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])]

  def mapK[G[_]](h: [A] => F[A] => G[A]): SchemaLookup[G] =
    new SchemaLookup {
      override def lookup(schemaName: String): Exists[[T] =>> (Type[T], G[Expr[Schema[T]]])] =
        SchemaLookup.this.lookup(schemaName) match
          case Indeed((t, e)) => Indeed((t, h(e)))
    }
}

object SchemaLookup {
  def fromMap[F[_]](using Quotes)(
    schemas: Map[String, Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])]],
  ): SchemaLookup[F] =
    SchemaMap[F](schemas)

  private class SchemaMap[F[_]](using Quotes)(
    schemas: Map[String, Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])]],
  ) extends SchemaLookup[F] {
    override def lookup(schemaName: String): Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])] =
      schemas(schemaName)
  }
}
