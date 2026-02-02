enablePlugins(Fs2Grpc)
scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
  "io.grpc" % "grpc-netty-shaded" % "1.58.0",
  "org.typelevel" %% "cats-effect" % "3.5.2",
  "org.yaml" % "snakeyaml" % "2.2"
)

// ПУТЬ ИСПРАВЛЕН: один уровень вверх (../)
Compile / PB.protoSources := Seq(baseDirectory.value / "../api/proto/v1")

Compile / mainClass := Some("ebusta.dsl.Main")
assembly / mainClass := Some("ebusta.dsl.Main")

// ПУТЬ ИСПРАВЛЕН: один уровень вверх (../)
assembly / assemblyOutputPath := baseDirectory.value / "../cmd/dsl-scala/dsl-server.jar"

assembly / assemblyMergeStrategy := {
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
  case x => MergeStrategy.defaultMergeStrategy(x)
}
