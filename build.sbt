// Scalafix configuration
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies ++= Seq(
  "com.github.vovapolu" %% "scaluzzi" % "0.1.21"
)

// SCoverage configuration
ThisBuild / coverageFailOnMinimum := true
ThisBuild / coverageMinimumStmtTotal := 80
ThisBuild / coverageMinimumBranchTotal := 80
ThisBuild / coverageMinimumStmtPerPackage := 80
ThisBuild / coverageMinimumBranchPerPackage := 80
ThisBuild / coverageMinimumStmtPerFile := 50
ThisBuild / coverageMinimumBranchPerFile := 50

// -- Lib versions
lazy val libVersion = new {
  val zio = "2.0.0-RC1"
}

// -- Main project settings
lazy val core =
  (project in file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"          %% "zio-test"     % libVersion.zio       % Test,
      "dev.zio"          %% "zio-test-sbt" % libVersion.zio       % Test,
      "dev.zio"          %% "zio"          % libVersion.zio,
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
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
    name        := "Crier",
    version     := "0.1.0",
    description := "Take pages from notion and post them on Linkedin and Twitter daily",
    startYear   := Some(2022),
    licenses    += ("Apache-2.0" → new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage    := Some(url("https://github.com/univalence/crier")),
    // -- Contributors
    developers := List(
      Developer(
        id = "dylandoamaral",
        name = "Dylan Do Amaral",
        email = "dylan@univalence.io",
        url = url("https://github.com/dylandoamaral")
      )
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/univalence/crier"),
        "scm:git:https://github.com/univalence/crier.git",
        "scm:git:git@github.com:univalence/crier.git"
      ))
  )

lazy val scalaSettings =
  Def.settings(
    crossScalaVersions := Seq("2.13.8"),
    scalaVersion       := crossScalaVersions.value.head,
  )
