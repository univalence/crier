// Scala configuration
ThisBuild / crossScalaVersions         := Seq("2.13.8")
ThisBuild / scalaVersion               := crossScalaVersions.value.head
ThisBuild / scalafixScalaBinaryVersion := "2.13"

// Scalafix configuration
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies ++= Seq(
  "com.github.vovapolu" %% "scaluzzi" % "0.1.21"
)

// SCoverage configuration
ThisBuild / coverageFailOnMinimum           := true
ThisBuild / coverageMinimumStmtTotal        := 80
ThisBuild / coverageMinimumBranchTotal      := 80
ThisBuild / coverageMinimumStmtPerPackage   := 80
ThisBuild / coverageMinimumBranchPerPackage := 80
ThisBuild / coverageMinimumStmtPerFile      := 50
ThisBuild / coverageMinimumBranchPerFile    := 50

// -- Lib versions
val zio       = "2.0.0-RC6"
val zioConfig = "3.0.0-RC9"
val sttp      = "3.6.2"
val circe     = "0.14.1"
val sl4j      = "1.7.36"

// -- Main project settings
lazy val core =
  (project in file("core"))
    .enablePlugins(JavaAppPackaging)
    .settings(
      name                := "crier",
      Compile / mainClass := Some("io.univalence.crier.Main"),
      libraryDependencies ++= Seq(
        "dev.zio"                       %% "zio-test"                      % zio % Test,
        "dev.zio"                       %% "zio-test-sbt"                  % zio % Test,
        "dev.zio"                       %% "zio"                           % zio,
        "dev.zio"                       %% "zio-config"                    % zioConfig,
        "dev.zio"                       %% "zio-config-magnolia"           % zioConfig,
        "com.softwaremill.sttp.client3" %% "core"                          % sttp,
        "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttp,
        "com.softwaremill.sttp.client3" %% "circe"                         % sttp,
        "io.circe"                      %% "circe-generic"                 % circe,
        "io.circe"                      %% "circe-generic-extras"          % circe,
        "org.slf4j"                      % "slf4j-simple"                  % sl4j
      ),
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
      scalacOptions ++= Seq("-Ymacro-annotations")
    )

lazy val benchmark =
  (project in file("benchmark"))
    .enablePlugins(JmhPlugin)
    .dependsOn(core)

lazy val metadataSettings =
  Def.settings(
    // -- Organization
    organization         := "io.univalence",
    organizationName     := "Univalence",
    organizationHomepage := Some(url("https://univalence.io/")),
    // -- Project
    name                     := "Crier",
    version                  := "0.1.0",
    description              := "Take pages from notion and post them on Linkedin daily",
    startYear                := Some(2022),
    licenses += ("Apache-2.0" ??? new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage                 := Some(url("https://github.com/univalence/crier")),
    // -- Contributors
    developers := List(
      Developer(
        id    = "dylandoamaral",
        name  = "Dylan Do Amaral",
        email = "dylan@univalence.io",
        url   = url("https://github.com/dylandoamaral")
      )
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/univalence/crier"),
        "scm:git:https://github.com/univalence/crier.git",
        "scm:git:git@github.com:univalence/crier.git"
      )
    )
  )
