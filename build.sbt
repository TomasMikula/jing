ThisBuild / scalaVersion := "3.6.3"

ThisBuild / organization := "dev.continuously.jing"

ThisBuild / licenses += ("MPL 2.0", url("https://opensource.org/licenses/MPL-2.0"))
ThisBuild / homepage := Some(url("https://github.com/TomasMikula/jing"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/TomasMikula/jing"),
    "scm:git:git@github.com:TomasMikula/jing.git"
  )
)

lazy val jingOpenApiModel = project
  .in(file("jing-openapi-model"))

lazy val jingOpenApi = project
  .in(file("jing-openapi"))
  .dependsOn(
    jingOpenApiModel,
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
      "io.swagger.parser.v3" % "swagger-parser" % "2.1.25",
    ),
    scalacOptions ++= Seq(
      "-experimental",
    ),
  )

lazy val jingOpenApiExamples = project
  .in(file("jing-openapi-examples"))
  .dependsOn(
    jingOpenApi % Provided,
  )
  .settings(
    scalacOptions ++= Seq(
      "-experimental",
    ),
  )
