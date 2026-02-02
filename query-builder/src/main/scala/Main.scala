package ebusta.querybuilder

import cats.effect.*
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import fs2.grpc.syntax.all.*
import io.grpc.Metadata
// ВАЖНО: Импорт Reflection
import io.grpc.protobuf.services.ProtoReflectionService

import io.circe.*
import io.circe.syntax.*
import io.circe.parser.*

import java.io.{FileWriter, FileInputStream}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Map as JMap
import org.yaml.snakeyaml.Yaml

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
  def build(ast: SearchQuery, size: Int, from: Int): (Json, QueryType) = {
    ast.query match {
      case SearchQuery.Query.Filter(filter) =>
        val (tplId, params) = mapToTemplate(filter.field, filter.value)
        val fullParams = params ++ Map("size" -> size.toString, "from" -> from.toString)
        (Json.obj("id" -> tplId.asJson, "params" -> fullParams.asJson), QueryType.TEMPLATE)
      case SearchQuery.Query.Logical(_) =>
        val dslQuery = OsDslBuilder.toDsl(ast)
        (Json.obj("size" -> size.asJson, "from" -> from.asJson, "query" -> dslQuery), QueryType.DSL)
      case _ => (Json.obj(), QueryType.DSL)
    }
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
  def getConfigPath(): String = sys.env.getOrElse("EBUSTA_CONFIG", "ebusta.yaml")
   
  def loadConfig(path: String): (String, Int) = {
    val input = new FileInputStream(path)
    val yaml = new Yaml()
    val data = yaml.load(input).asInstanceOf[JMap[String, Any]]
    val section = data.get("query_builder").asInstanceOf[JMap[String, Any]]
    (section.get("host").toString, section.get("port").toString.toDouble.toInt)
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

    initStep.flatMap { case (host, port) =>
      val serverResource = for {
        _ <- Resource.eval(logger.info(s"Listening on $host:$port"))
        service <- QueryBuilderFs2Grpc.bindServiceResource[IO](new QueryBuilderImpl(logger))
        server <- Resource.make(
          IO(
            NettyServerBuilder.forPort(port)
              .addService(service)
              .addService(ProtoReflectionService.newInstance()) // <--- ВКЛЮЧАЕМ REFLECTION
              .build()
              .start()
          )
        )(s => logger.info("=== STOPPING ===") *> IO(s.shutdown().awaitTermination()).void)
      } yield server

      serverResource.use(_ => IO.never).as(ExitCode.Success)
    }
  }
}
