name := "query-builder"
version := "0.1"
scalaVersion := "3.3.1"

enablePlugins(Fs2Grpc)

Compile / unmanagedSourceDirectories += baseDirectory.value / "../../query-builder/src/main/scala"

val grpcVersion = "1.59.0"

libraryDependencies ++= Seq(
  "org.typelevel"        %% "cats-effect"          % "3.5.0",
  "co.fs2"               %% "fs2-core"             % "3.9.3",
  "org.typelevel"        %% "fs2-grpc-runtime"     % "2.7.16",
  "org.yaml"             %  "snakeyaml"            % "2.2",
  "io.circe"             %% "circe-core"           % "0.14.6",
  "io.circe"             %% "circe-generic"        % "0.14.6",
  "io.circe"             %% "circe-parser"         % "0.14.6",
  "com.thesamet.scalapb" %% "scalapb-runtime"      % "0.11.13" % "protobuf",
  "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % "0.11.13",
  "io.grpc"              %  "grpc-netty-shaded"    % grpcVersion,
  "io.grpc"              %  "grpc-core"            % grpcVersion,
  "io.grpc"              %  "grpc-api"             % grpcVersion,
  // ДОБАВЛЕНО ДЛЯ GRPCURL:
  "io.grpc"              %  "grpc-services"        % grpcVersion
)

dependencyOverrides ++= Seq(
  "io.grpc" % "grpc-core" % grpcVersion,
  "io.grpc" % "grpc-api" % grpcVersion,
  "io.grpc" % "grpc-netty-shaded" % grpcVersion,
  "io.grpc" % "grpc-context" % grpcVersion,
  "io.grpc" % "grpc-protobuf" % grpcVersion,
  "io.grpc" % "grpc-stub" % grpcVersion
)

Compile / PB.protoSources += baseDirectory.value / "../../api/proto/v1"

Compile / PB.targets := (Compile / PB.targets).value.map { target =>
  if (target.outputPath.getName == "scalapb") {
     val (newGen, newOpts) = scalapb.gen(grpc = true)
     protocbridge.Target(newGen, target.outputPath, newOpts)
  } else {
     target
  }
}

assembly / assemblyJarName := "query-builder.jar"
assembly / mainClass := Some("ebusta.querybuilder.Main")

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", "versions", xs @ _*) => MergeStrategy.discard
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) =>
    xs map {_.toLowerCase} match {
      case "services" :: xs => MergeStrategy.filterDistinctLines
      case _ => MergeStrategy.discard
    }
  case _ => MergeStrategy.first
}
