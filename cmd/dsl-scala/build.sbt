enablePlugins(PekkoGrpcPlugin)

name := "dsl-scala-transformer"
version := "0.1"
scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
  "org.apache.pekko"       %% "pekko-actor-typed"       % "1.0.2",
  "org.apache.pekko"       %% "pekko-stream"            % "1.0.2",
  "org.apache.pekko"       %% "pekko-http"              % "1.0.1"
)

Compile / PB.protoSources += baseDirectory.value / "../../api/proto/v1"
