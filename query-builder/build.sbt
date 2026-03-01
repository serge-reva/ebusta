enablePlugins(Fs2Grpc)
scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.5.2",
  "io.grpc" % "grpc-netty-shaded" % "1.58.0",
  "io.grpc" % "grpc-services" % "1.58.0",
  "org.yaml" % "snakeyaml" % "2.2",
  "com.github.ben-manes.caffeine" % "caffeine" % "3.1.8",
  "io.circe" %% "circe-core" % "0.14.6",
  "io.circe" %% "circe-generic" % "0.14.6",
  "io.circe" %% "circe-parser" % "0.14.6",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

// ПУТЬ ИСПРАВЛЕН: один уровень вверх (../)
Compile / PB.protoSources := Seq(baseDirectory.value / "../api/proto/v1")

Compile / mainClass := Some("ebusta.querybuilder.Main")
assembly / mainClass := Some("ebusta.querybuilder.Main")

// ПУТЬ ИСПРАВЛЕН: один уровень вверх (../)
assembly / assemblyOutputPath := baseDirectory.value / "../cmd/query-builder/query-builder.jar"

assembly / assemblyMergeStrategy := {
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
  case x => MergeStrategy.defaultMergeStrategy(x)
}
