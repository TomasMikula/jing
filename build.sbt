ThisBuild / scalaVersion := "3.7.4"

ThisBuild / organization := "dev.continuously.jing"

ThisBuild / licenses += ("MPL 2.0", url("https://opensource.org/licenses/MPL-2.0"))
ThisBuild / homepage := Some(url("https://github.com/TomasMikula/jing"))
ThisBuild / description := "Just Import 'N' Go: Spec-first APIs without codegen"
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/TomasMikula/jing"),
    "scm:git:git@github.com:TomasMikula/jing.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id = "TomasMikula",
    name = "Tomas Mikula",
    email = "tomas.mikula@continuously.dev",
    url = url("https://continuously.dev")
  ),
)

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true

ThisBuild / publishTo := {
  if (isSnapshot.value)
    Some("central-snapshots" at "https://central.sonatype.com/repository/maven-snapshots/")
  else
    localStaging.value
}

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  releaseStepCommand("sonaRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges,
)

val scalacOptionsCommon =
  Seq(
    "-deprecation",
    "-Wunused:all",
    "-Xkind-projector:underscores",
  )

// ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots

val LibrettoVersion = "0.3.7"
val Http4sVersion = "0.23.33"
val ScalatestVersion = "3.2.19"

lazy val macroUtil = project
  .in(file("macro-util"))
  .settings(
    name := "jing-macro-util",
    libraryDependencies ++= Seq(
      "dev.continuously.libretto" %% "libretto-lambda" % LibrettoVersion,
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    ),
    scalacOptions ++= scalacOptionsCommon,
  )

lazy val jingOpenApiModel = project
  .in(file("jing-openapi-model"))
  .dependsOn(
    macroUtil,
  )
  .settings(
    name := "jing-openapi-model",
    scalacOptions ++= scalacOptionsCommon,
    libraryDependencies ++= Seq(
      "dev.continuously.libretto" %% "libretto-lambda" % LibrettoVersion,
      "io.circe" %% "circe-parser" % "0.14.14",
    ),
  )

lazy val jingOpenApi = project
  .in(file("jing-openapi"))
  .dependsOn(
    jingOpenApiModel,
    macroUtil,
  )
  .settings(
    name := "jing-openapi",
    libraryDependencies ++= Seq(
      "io.swagger.parser.v3" % "swagger-parser" % "2.1.35",
      "org.scalatest" %% "scalatest" % ScalatestVersion % "test",
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
    name := "jing-openapi-client-default",
    scalacOptions ++=
      scalacOptionsCommon,
  )

lazy val jingOpenApiServerHttp4s = project
  .in(file("jing-openapi-server-http4s"))
  .dependsOn(
    jingOpenApiModel,
  )
  .settings(
    name := "jing-openapi-server-http4s",
    scalacOptions ++=
      scalacOptionsCommon,
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-core" % Http4sVersion,
    ),
  )

lazy val jingOpenApiExamples = project
  .in(file("jing-openapi-examples"))
  .dependsOn(
    jingOpenApiModel,
    jingOpenApiClientDefault,
    jingOpenApiServerHttp4s,
    jingOpenApi % Provided,
  )
  .settings(
    name := "jing-openapi-examples",
    scalacOptions ++=
      scalacOptionsCommon ++
      Seq(
        "-experimental",
      ),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % Http4sVersion,
    ),
    publish / skip := true,

  )
