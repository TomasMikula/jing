package jing.openapi

import jing.openapi.model.Schema
import libretto.lambda.util.{Exists, TypeEq}
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq.Refl
import scala.quoted.*

trait SchemaLookup[Rel[_, _], F[_]] {

  def lookupEntry[A](
    schemaName: String,
    sourceSchema: Schema.Labeled[String, A],
  )(using
    Quotes,
  ): Exists[[B] =>> (Rel[A, B], Type[B], F[Expr[Schema[B]]])]

  def mapK[G[_]](h: [A] => F[A] => G[A]): SchemaLookup[Rel, G] =
    new SchemaLookup {
      override def lookupEntry[A](
        schemaName: String,
        sourceSchema: Schema.Labeled[String, A],
      )(using Quotes): Exists[[B] =>> (Rel[A, B], Type[B], G[Expr[Schema[B]]])] =
        SchemaLookup.this.lookupEntry(schemaName, sourceSchema) match
          case Indeed((rel, t, expr)) => Indeed((rel, t, h(expr)))
    }
}

object SchemaLookup {
  sealed trait Entry[Rel[_, _], F[_]] {
    def mapK[G[_]](h: [A] => F[A] => G[A]): Entry[Rel, G]
  }

  object Entry:
    private[SchemaLookup] case class Impl[Rel[_, _], F[_], A, B](
      sourceSchema: Schema.Labeled[String, A],
      rel: Rel[A, B],
      aliasedType: Type[B],
      schemaExpr: F[Expr[Schema[B]]],
    ) extends Entry[Rel, F] {
      override def mapK[G[_]](h: [X] => F[X] => G[X]): Entry[Rel, G] =
        Impl(sourceSchema, rel, aliasedType, h(schemaExpr))
    }

    def apply[Rel[_, _], F[_], A, B](
      sourceSchema: Schema.Labeled[String, A],
      rel: Rel[A, B],
      aliasedType: Type[B],
      schemaExpr: F[Expr[Schema[B]]],
    ): Entry[Rel, F] =
      Impl(sourceSchema, rel, aliasedType, schemaExpr)

  def fromMap[Rel[_, _], F[_]](
    schemas: Map[String, Entry[Rel, F]],
  ): SchemaLookup[Rel, F] =
    SchemaMap[Rel, F](schemas)

  private class SchemaMap[Rel[_, _], F[_]](
    schemas: Map[String, Entry[Rel, F]],
  ) extends SchemaLookup[Rel, F] {

    override def lookupEntry[A](
      schemaName: String,
      sourceSchema: Schema.Labeled[String, A],
    )(using q: Quotes): Exists[[B] =>> (Rel[A, B], Type[B], F[Expr[Schema[B]]])] =
      schemas.get(schemaName) match
        case Some(Entry.Impl(sourceSchema0, rel, t, expr)) =>
          (sourceSchema0 isEqualTo sourceSchema) match
            case Some(TypeEq(Refl())) =>
              Indeed(rel, t, expr)
            case None =>
              q.reflect.report.errorAndAbort(s"Schema named '$schemaName' does not match the expected schema. Expected: $sourceSchema. Actual: $sourceSchema0")
        case None =>
          q.reflect.report.errorAndAbort(s"Schema named '$schemaName' not found")

  }
}
