package jing

import scala.quoted.{Expr, Quotes, quotes}

package object openapi {

  transparent inline def apply(inline url: String) =
    ${ go('url) }

  transparent inline def inlineYaml(inline yaml: String) =
    ${ inlineYamlImpl('yaml) }

  private def go(urlExpr: Expr[String])(using Quotes): Expr[Any] = {
    val url = urlExpr.valueOrAbort

    SwaggerToScalaAst(url)
  }

  private def inlineYamlImpl(yamlExpr: Expr[String])(using Quotes): Expr[Any] = {
    val yaml = yamlExpr.valueOrAbort
    val pos = quotes.reflect.Position.ofMacroExpansion
    val location = s"${pos.sourceFile.path}:${pos.startLine}"

    SwaggerToScalaAst.fromString(yaml)(location)
  }

}

