package ebusta.querybuilder

import cats.effect.*
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts
import io.grpc.netty.shaded.io.netty.handler.ssl.ClientAuth
import fs2.grpc.syntax.all.*
import io.grpc.Metadata
// ВАЖНО: Импорт Reflection
import io.grpc.protobuf.services.ProtoReflectionService
import io.grpc.protobuf.services.HealthStatusManager
import io.grpc.health.v1.HealthCheckResponse.ServingStatus

import io.circe.*
import io.circe.syntax.*

import java.io.{FileWriter, FileInputStream}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicBoolean
import java.util.Map as JMap
import org.yaml.snakeyaml.Yaml
import io.prometheus.client.exporter.HTTPServer
import io.prometheus.client.hotspot.DefaultExports

import ebusta.qb.v1.query_builder.*
import ebusta.library.v1.common.*

class SimpleLogger(verbose: Boolean, logFilePath: String = "qb.log") {
  private val dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
  def log(level: String, msg: String): IO[Unit] = IO.blocking {
    val timestamp = LocalDateTime.now().format(dtf)
    val line = s"[$timestamp] [$level] $msg"
    this.synchronized {
      val fw = new FileWriter(logFilePath, true); try fw.write(line + "\n") finally fw.close()
    }
    if (verbose) println(line)
  }
  def info(msg: String): IO[Unit] = log("INFO", msg)
  def error(msg: String): IO[Unit] = log("ERROR", msg)
}

object OsDslBuilder {
  def toDsl(query: SearchQuery): Json = {
    query.query match {
      case SearchQuery.Query.Logical(logical) => buildLogical(logical)
      case SearchQuery.Query.Filter(filter)   => buildFilter(filter)
      case _ => Json.obj("match_all" -> Json.obj())
    }
  }

  private def buildLogical(node: LogicalNode): Json = {
    val clauses = node.nodes.map(toDsl)
    node.op match {
      case 1 => Json.obj("bool" -> Json.obj("must" -> Json.fromValues(clauses)))
      case 2 => Json.obj("bool" -> Json.obj("should" -> Json.fromValues(clauses), "minimum_should_match" -> 1.asJson))
      case 3 => Json.obj("bool" -> Json.obj("must_not" -> clauses.headOption.getOrElse(Json.obj())))
      case _ => Json.obj()
    }
  }

  private def buildFilter(node: FilterNode): Json = {
    node.field match {
      case "author_exact" =>
        Json.obj("match_phrase" -> Json.obj("authors" -> Json.obj("query" -> node.value.asJson)))
      case "title_exact" =>
        Json.obj("match_phrase" -> Json.obj("title" -> Json.obj("query" -> node.value.asJson)))
      case "_all_exact" =>
        Json.obj("bool" -> Json.obj(
          "should" -> Json.arr(
            Json.obj("match_phrase" -> Json.obj("title" -> Json.obj("query" -> node.value.asJson, "boost" -> 5.asJson))),
            Json.obj("match_phrase" -> Json.obj("authors" -> Json.obj("query" -> node.value.asJson, "boost" -> 4.asJson))),
            Json.obj("term" -> Json.obj("title.kw" -> node.value.asJson)),
            Json.obj("term" -> Json.obj("authors.kw" -> node.value.asJson))
          ),
          "minimum_should_match" -> 1.asJson
        ))
      case "author" =>
        Json.obj("bool" -> Json.obj("should" -> Json.arr(
          Json.obj("term" -> Json.obj("authors.kw" -> Json.obj("value" -> node.value.asJson, "boost" -> 10.asJson))),
          Json.obj("match_phrase" -> Json.obj("authors" -> Json.obj("query" -> node.value.asJson, "boost" -> 5.asJson))),
          Json.obj("match" -> Json.obj("authors" -> Json.obj("query" -> node.value.asJson, "boost" -> 2.asJson, "operator" -> "and".asJson))),
          Json.obj("match_phrase_prefix" -> Json.obj("authors" -> Json.obj("query" -> node.value.asJson, "boost" -> 1.asJson)))
        ), "minimum_should_match" -> 1.asJson))
      case "title" =>
        Json.obj("bool" -> Json.obj("should" -> Json.arr(
          Json.obj("term" -> Json.obj("title.kw" -> Json.obj("value" -> node.value.asJson, "boost" -> 10.asJson))),
          Json.obj("match_phrase" -> Json.obj("title" -> Json.obj("query" -> node.value.asJson, "boost" -> 5.asJson))),
          Json.obj("match" -> Json.obj("title" -> Json.obj("query" -> node.value.asJson, "boost" -> 2.asJson, "operator" -> "and".asJson))),
          Json.obj("match_phrase_prefix" -> Json.obj("title" -> Json.obj("query" -> node.value.asJson, "boost" -> 1.asJson)))
        ), "minimum_should_match" -> 1.asJson))
      case "id" => Json.obj("term" -> Json.obj("_id" -> node.value.asJson))
      case "container" => Json.obj("term" -> Json.obj("fileInfo.container" -> node.value.asJson))
      case "filename" => Json.obj("wildcard" -> Json.obj("fileInfo.filename" -> ("*" + node.value + "*").asJson))
      case _ =>
         Json.obj("bool" -> Json.obj("should" -> Json.arr(
           Json.obj("term" -> Json.obj("title.kw" -> Json.obj("value" -> node.value.asJson, "boost" -> 10.asJson))),
           Json.obj("term" -> Json.obj("authors.kw" -> Json.obj("value" -> node.value.asJson, "boost" -> 8.asJson))),
           Json.obj("multi_match" -> Json.obj(
             "query" -> node.value.asJson,
             "fields" -> Json.arr("title^5".asJson, "authors^3".asJson, "annotation^1".asJson, "fileInfo.filename^1".asJson),
             "type" -> "best_fields".asJson,
             "operator" -> "and".asJson,
             "fuzziness" -> "AUTO".asJson
           ))
         ), "minimum_should_match" -> 1.asJson))
    }
  }
}

object OsStrategy {
  private def isExactField(field: String): Boolean = field == "author_exact" || field == "title_exact" || field == "_all_exact"

  private def cacheKey(ast: SearchQuery): String = ast.toProtoString

  private def addPagination(base: Json, qType: QueryType, size: Int, from: Int): Json = {
    qType match {
      case QueryType.TEMPLATE =>
        val paramsObj = base.hcursor.downField("params").focus.flatMap(_.asObject).getOrElse(JsonObject.empty)
        base.deepMerge(
          Json.obj(
            "params" -> Json.fromJsonObject(
              paramsObj
                .add("size", size.toString.asJson)
                .add("from", from.toString.asJson)
            )
          )
        )
      case _ =>
        Json.obj("size" -> size.asJson, "from" -> from.asJson).deepMerge(base)
    }
  }

  private def buildBase(ast: SearchQuery): (Json, QueryType) = {
    ast.query match {
      case SearchQuery.Query.Filter(filter) =>
        if (isExactField(filter.field)) {
          val dslQuery = OsDslBuilder.toDsl(ast)
          (Json.obj("query" -> dslQuery), QueryType.DSL)
        } else {
          val (tplId, params) = mapToTemplate(filter.field, filter.value)
          (Json.obj("id" -> tplId.asJson, "params" -> params.asJson), QueryType.TEMPLATE)
        }
      case SearchQuery.Query.Logical(_) =>
        val dslQuery = OsDslBuilder.toDsl(ast)
        (Json.obj("query" -> dslQuery), QueryType.DSL)
      case _ => (Json.obj(), QueryType.DSL)
    }
  }

  def build(ast: SearchQuery, size: Int, from: Int): (Json, QueryType) = {
    val (base, qType) = QueryCache.getOrCompute(cacheKey(ast)) {
      buildBase(ast)
    }
    (addPagination(base, qType, size, from), qType)
  }

  private def mapToTemplate(field: String, value: String): (String, Map[String, String]) = field match {
    case "author"    => ("fl_smart_author", Map("query" -> value))
    case "title"     => ("fl_smart_title",  Map("query" -> value))
    case "id"        => ("fl_id_lookup",    Map("query" -> value))
    case "container" => ("fl_container_lookup", Map("query" -> value))
    case "filename"  => ("fl_filename_lookup",  Map("query" -> value))
    case _           => ("fl_smart_general", Map("query" -> value))
  }
}

class QueryBuilderImpl(logger: SimpleLogger) extends QueryBuilderFs2Grpc[IO, Metadata] {
  override def build(req: BuildRequest, ctx: Metadata): IO[BuildResponse] = {
    for {
      _ <- logger.info(s"--> REQ: size=${req.size}")
      result <- IO {
        req.ast match {
          case Some(ast) =>
            val (json, qType) = OsStrategy.build(ast, req.size, req.from)
            BuildResponse(bodyJson = json.noSpaces, `type` = qType, isSuccess = true)
          case None =>
            BuildResponse(isSuccess = false, errorMsg = "AST is empty")
        }
      }.handleErrorWith(e => IO.pure(BuildResponse(isSuccess = false, errorMsg = e.getMessage)))
      _ <- logger.info(s"<-- RES: success=${result.isSuccess} type=${result.`type`}")
    } yield result
  }
}

object Main extends IOApp {
  case class RuntimeConfig(
    host: String,
    port: Int,
    cacheMaxSize: Long,
    cacheTtlSeconds: Long,
    metricsPort: Int,
    tlsEnabled: Boolean,
    tlsCaFile: String,
    tlsCertFile: String,
    tlsKeyFile: String
  )

  def getConfigPath(): String = sys.env.getOrElse("EBUSTA_CONFIG", "ebusta.yaml")
   
  def loadConfig(path: String): RuntimeConfig = {
    val input = new FileInputStream(path)
    val yaml = new Yaml()
    val data = yaml.load(input).asInstanceOf[JMap[String, Any]]
    val section = data.get("query_builder").asInstanceOf[JMap[String, Any]]
    val host = section.get("host").toString
    val port = section.get("port").toString.toDouble.toInt

    val cacheSection = Option(section.get("cache")).map(_.asInstanceOf[JMap[String, Any]])
    val cacheMaxSize = cacheSection
      .flatMap(s => Option(s.get("max_size")))
      .orElse(Option(section.get("cache_max_size")))
      .map(_.toString.toDouble.toLong)
      .getOrElse(1000L)

    val cacheTtlSeconds = cacheSection
      .flatMap(s => Option(s.get("ttl_seconds")))
      .orElse(Option(section.get("cache_ttl_seconds")))
      .map(_.toString.toDouble.toLong)
      .getOrElse(60L)

    val metricsSection = Option(data.get("metrics")).map(_.asInstanceOf[JMap[String, Any]])
    val metricsServices = metricsSection.flatMap(m => Option(m.get("services")).map(_.asInstanceOf[JMap[String, Any]]))
    val metricsPort = metricsServices.flatMap(s => Option(s.get("query_builder")).map(_.toString.toDouble.toInt)).getOrElse(59053)
    val mtlsSection = Option(section.get("mtls")).map(_.asInstanceOf[JMap[String, Any]])
    val tlsEnabled = mtlsSection.flatMap(s => Option(s.get("enabled"))).exists(_.toString.toBoolean)
    val tlsCaFile = mtlsSection.flatMap(s => Option(s.get("ca_file"))).map(_.toString).getOrElse("")
    val tlsCertFile = mtlsSection.flatMap(s => Option(s.get("cert_file"))).map(_.toString).getOrElse("")
    val tlsKeyFile = mtlsSection.flatMap(s => Option(s.get("key_file"))).map(_.toString).getOrElse("")
    input.close()

    RuntimeConfig(host, port, cacheMaxSize, cacheTtlSeconds, metricsPort, tlsEnabled, tlsCaFile, tlsCertFile, tlsKeyFile)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val isVerbose = args.contains("-v") || args.contains("--verbose")
    val logger = new SimpleLogger(isVerbose)
    val configPath = getConfigPath()

    val initStep = for {
      _ <- logger.info("=== QUERY BUILDER STARTING ===")
      config <- IO(loadConfig(configPath)).handleErrorWith { e =>
         logger.error(s"Config error: ${e.getMessage}") *> IO.raiseError(e)
      }
    } yield config

    initStep.flatMap { cfg =>
      val serverResource = for {
        _ <- Resource.eval(logger.info(s"Listening on ${cfg.host}:${cfg.port}"))
        _ <- Resource.eval(if (cfg.tlsEnabled) logger.info("mTLS mode enabled") else logger.info("mTLS mode disabled"))
        _ <- Resource.eval(IO(QueryCache.configure(cfg.cacheMaxSize, cfg.cacheTtlSeconds)))
        _ <- Resource.eval(logger.info(s"Cache configured: max_size=${cfg.cacheMaxSize} ttl_seconds=${cfg.cacheTtlSeconds}"))
        _ <- Resource.eval(IO {
          DefaultExports.initialize()
          new HTTPServer(cfg.metricsPort)
        })
        _ <- Resource.eval(logger.info(s"Metrics listening on :${cfg.metricsPort}"))
        service <- QueryBuilderFs2Grpc.bindServiceResource[IO](new QueryBuilderImpl(logger))
        health = new HealthStatusManager()
        _ <- Resource.eval(IO(health.setStatus("", ServingStatus.SERVING)))
        server <- Resource.make(
          IO {
            val baseBuilder = NettyServerBuilder.forPort(cfg.port)
              .addService(service)
              .addService(health.getHealthService)
              .addService(ProtoReflectionService.newInstance())
            val configuredBuilder =
              if (cfg.tlsEnabled) {
                val sslCtx = GrpcSslContexts
                  .forServer(new java.io.File(cfg.tlsCertFile), new java.io.File(cfg.tlsKeyFile))
                  .trustManager(new java.io.File(cfg.tlsCaFile))
                  .clientAuth(ClientAuth.REQUIRE)
                  .build()
                baseBuilder.sslContext(sslCtx)
              } else {
                baseBuilder
              }
            configuredBuilder.build().start()
          }
        )(s => logger.info("=== STOPPING ===") *> IO(s.shutdown().awaitTermination()).void)
      } yield server

      serverResource.use { server =>
        val hookInstalled = new AtomicBoolean(false)
        for {
          _ <- IO {
            if (hookInstalled.compareAndSet(false, true)) {
              Runtime.getRuntime.addShutdownHook(new Thread(() => {
                server.shutdown()
                server.awaitTermination()
              }))
            }
          }
          _ <- IO.blocking(server.awaitTermination()).void
        } yield ()
      }.as(ExitCode.Success)
    }
  }
}
