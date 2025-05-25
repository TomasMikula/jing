ThisBuild / scalaVersion := "3.7.0"

ThisBuild / organization := "dev.continuously.jing"

ThisBuild / licenses += ("MPL 2.0", url("https://opensource.org/licenses/MPL-2.0"))
ThisBuild / homepage := Some(url("https://github.com/TomasMikula/jing"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/TomasMikula/jing"),
    "scm:git:git@github.com:TomasMikula/jing.git"
  )
)

val scalacOptionsCommon =
  Seq(
    "-deprecation",
    "-Xkind-projector:underscores",
  )

lazy val jingOpenApiModel = project
  .in(file("jing-openapi-model"))
  .settings(
    scalacOptions ++= scalacOptionsCommon,
    libraryDependencies ++= Seq(
      "dev.continuously.libretto" %% "libretto-lambda" % "0.3.6-SNAPSHOT",
    )
  )

lazy val jingOpenApi = project
  .in(file("jing-openapi"))
  .dependsOn(
    jingOpenApiModel,
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
      "io.swagger.parser.v3" % "swagger-parser" % "2.1.26",
    ),
    scalacOptions ++=
      scalacOptionsCommon ++
      Seq(
        "-experimental",
      ),
  )

lazy val jingOpenApiClientDefault = project
  .in(file("jing-openapi-client-default"))
  .dependsOn(
    jingOpenApiModel,
  )
  .settings(
    scalacOptions ++=
      scalacOptionsCommon,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-parser" % "0.14.12",
    ),
  )

lazy val jingOpenApiExamples = project
  .in(file("jing-openapi-examples"))
  .dependsOn(
    jingOpenApiModel,
    jingOpenApiClientDefault,
    jingOpenApi % Provided,
  )
  .settings(
    scalacOptions ++=
      scalacOptionsCommon ++
      Seq(
        "-experimental",
      ),
  )
