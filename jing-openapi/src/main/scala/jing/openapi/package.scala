package jing

import scala.quoted.{Expr, Quotes}

package object openapi {

  transparent inline def apply(inline url: String) =
    ${ go('url) }

  private def go(urlExpr: Expr[String])(using Quotes): Expr[Any] = {
    val url = urlExpr.valueOrAbort

    SwaggerToScalaAst(url)
  }

  private transparent inline def qr(using q: Quotes): q.reflect.type =
    q.reflect

}

