package jing.openapi.model

import libretto.lambda.util.SingletonType
import scala.quoted.*
import libretto.lambda.util.NonEmptyList

/** Witnesses that the type `T` is a union of singleton types. */
sealed trait UnionOfSingletons[T] {
  import UnionOfSingletons.*

  infix def union[U](that: UnionOfSingletons[U]): UnionOfSingletons[T | U] =
    that.unionAfter(this)

  protected def unionAfter[S](that: UnionOfSingletons[S]): UnionOfSingletons[S | T]
}

object UnionOfSingletons {
  case class Single[T](evidence: SingletonType[T]) extends UnionOfSingletons[T] {
    override protected def unionAfter[S](that: UnionOfSingletons[S]): UnionOfSingletons[S | T] =
      Snoc(that, evidence)
  }

  case class Snoc[T, U](
    init: UnionOfSingletons[T],
    last: SingletonType[U],
  ) extends UnionOfSingletons[T | U] {
    override protected def unionAfter[S](that: UnionOfSingletons[S]): UnionOfSingletons[S | (T | U)] =
      Snoc(init.unionAfter(that), last)
  }

  inline given instance[T]: UnionOfSingletons[T] =
    ${ instanceImpl[T] }

  private def instanceImpl[T](using Quotes, Type[T]): Expr[UnionOfSingletons[T]] =
    import quotes.reflect.*

    instanceImplOpt[T] match
      case Some(result) =>
        result
      case None =>
        report.errorAndAbort(s"Cannot prove that type ${Printer.TypeReprShortCode.show(TypeRepr.of[T])} is a union of singletons.")

  private def instanceImplOpt[T](using Quotes, Type[T]): Option[Expr[UnionOfSingletons[T]]] = {
    import quotes.reflect.*

    TypeRepr.of[T] match
      case ConstantType(c) =>
        val expr = Literal(c).asExpr
        val st = '{ SingletonType($expr) }.asExprOf[SingletonType[T]]
        Some:
          '{ Single($st) }
      case OrType(a, b) =>
        (a.asType, b.asType) match
          case ('[a], '[b]) =>
            for
              l <- instanceImplOpt[a]
              r <- instanceImplOpt[b]
            yield
              ('{ $l union $r }: Expr[UnionOfSingletons[a | b]])
                .asExprOf[UnionOfSingletons[T]]
  }
}
