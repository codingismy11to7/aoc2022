ThisBuild / scalaVersion := "2.13.8"
ThisBuild / scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation",
  "-Xlint:unused",
  "-Ymacro-annotations",
  "-feature",
)

Global / onChangedBuildSource := ReloadOnSourceChanges

val V = new {
  val betterMonadicFor = "0.3.1"
  val chimney          = "0.6.2"
  val enumeratum       = "1.7.0"
  val graph            = "1.13.5"
  val quicklens        = "1.8.10"
  val zio              = "2.0.4"
  val zioJson          = "0.3.0"
  val zioNio           = "2.0.0"
  val zioPrelude       = "1.0.0-RC16"
  val zioProcess       = "0.7.1"
}

(ThisBuild / libraryDependencies) += compilerPlugin("com.olegpy" %% "better-monadic-for" % V.betterMonadicFor)

lazy val noPublishSettings = Seq(
  publish                      := {},
  publishLocal                 := {},
  Compile / publishArtifact    := false,
  packageDoc / publishArtifact := false,
)

lazy val root = (project in file("."))
  .settings(
    name := "aoc",
    libraryDependencies ++= Seq(
      "com.beachape"               %% "enumeratum"  % V.enumeratum,
      "com.softwaremill.quicklens" %% "quicklens"   % V.quicklens,
      "dev.zio"                    %% "zio"         % V.zio,
      "dev.zio"                    %% "zio-json"    % V.zioJson,
      "dev.zio"                    %% "zio-macros"  % V.zio,
      "dev.zio"                    %% "zio-prelude" % V.zioPrelude,
      "io.scalaland"               %% "chimney"     % V.chimney,
    ),
    libraryDependencies ++= Seq(
      "dev.zio"         %% "zio-nio"      % V.zioNio,
      "dev.zio"         %% "zio-process"  % V.zioProcess,
      "dev.zio"         %% "zio-test"     % V.zio,
      "dev.zio"         %% "zio-test-sbt" % V.zio,
      "org.scala-graph" %% "graph-core"   % V.graph,
    ).map(_ % Test),
  )
