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
  def fromMap[F[_]](using Quotes, Applicative[F])(
    schemas: Map[String, (qr.TypeRepr, F[qr.Term])],
  ): SchemaLookup[F] =
    SchemaMap[F](schemas)

  private class SchemaMap[F[_]](using Quotes, Applicative[F])(
    schemas: Map[String, (qr.TypeRepr, F[qr.Term])],
  ) extends SchemaLookup[F] {
    override def lookup(schemaName: String): Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])] =
      val (tr, trm) = schemas(schemaName)
      def asSchemaExpr[T](trm: qr.Term)(using Type[T]): Expr[Schema[T]] =
        trm.asExprOf[Schema[T]]
      val tp = tr.asType.asInstanceOf[Type[Any]]
      Indeed((tp, trm.map(asSchemaExpr(_)(using tp))))
  }
}
