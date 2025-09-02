package jing.openapi

import jing.openapi.model.*
import org.scalatest.funsuite.AnyFunSuite

import scala.NamedTuple.NamedTuple

class GithubApiTest extends AnyFunSuite {

  // Allow up to 30s for this one to compile.
  // It is a small fragment of GitHub's v3 REST API, downloaded on 2025-08-31 from
  // https://raw.githubusercontent.com/github/rest-api-description/refs/heads/main/descriptions/api.github.com/api.github.com.json
  // Contains only (some) schemas, not paths or other components.
  // Larger fragments cause
  //     [error] Method too large: jing/openapi/GithubApiTest$$anon_schemas$4.<init> ()V
  // and even larger ones cause effective non-termination of the compiler.
  // The codegen using the reflect API is itself slow, but that part completes in 6-7 minutes on the full GitHub API.
  // It's the compilation of the resulting Expr that does not terminate in any reasonable time.
  test("fraction of GitHub API") {
    val api = jing.openapi("api.github.com-fragment.json")
  }

}
