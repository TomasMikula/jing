package jing.openapi

import jing.openapi.model.Schema
import libretto.lambda.util.Exists
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
  def forMode[M](using q: Quotes, mode: Mode[q.type, M])(
    types: Map[String, qr.TypeRepr],
    terms: mode.OutEff[Map[String, qr.Term]],
  ): SchemaLookup[mode.OutEff] =
    mode match
      case _: Mode.TypeSynth[q] => mode.outEffConstUnit.flip.subst(SchemaLookupForTypeSynth(types))
      case _: Mode.TermSynth[q] => mode.outEffId.flip.subst(SchemaLookupForTermSynth(types, mode.outEffId.at(terms)))

  private class SchemaLookupForTermSynth(using q: Quotes)(
    types: Map[String, qr.TypeRepr],
    terms: Map[String, qr.Term],
  ) extends SchemaLookup[[x] =>> x] {
    import quotes.reflect.*

    override def lookup(schemaName: String): Exists[[T] =>> (Type[T], Expr[Schema[T]])] =
      val tpe = types(schemaName).asType.asInstanceOf[Type[Any]]
      def go[T](using Type[T]): Expr[Schema[T]] =
        terms(schemaName)
          .asExprOf[Schema[T]]
      Exists((tpe, go(using tpe)))
  }

  private class SchemaLookupForTypeSynth(using q: Quotes)(
    types: Map[String, qr.TypeRepr],
  ) extends SchemaLookup[[x] =>> Unit] {
    override def lookup(schemaName: String): Exists[[T] =>> (Type[T], Unit)] =
      val tpe = types(schemaName).asType.asInstanceOf[Type[Any]]
      Exists((tpe, ()))
  }
}
