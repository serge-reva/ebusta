package ebusta.dsl

import cats.effect.*
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts
import io.grpc.netty.shaded.io.netty.handler.ssl.ClientAuth
import fs2.grpc.syntax.all.*
import scala.util.parsing.combinator.*
import io.grpc.Metadata
import io.grpc.protobuf.services.ProtoReflectionService
import io.grpc.protobuf.services.HealthStatusManager
import io.grpc.health.v1.HealthCheckResponse.ServingStatus

import java.io.{FileWriter, FileInputStream}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicBoolean
import java.util.Map as JMap
import org.yaml.snakeyaml.Yaml
import io.prometheus.client.exporter.HTTPServer
import io.prometheus.client.hotspot.DefaultExports

import ebusta.dsl.v1.dsl.*
import ebusta.library.v1.common.*

class SimpleLogger(verbose: Boolean, logFilePath: String = "server.log") {
  private val dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
  def log(level: String, msg: String): IO[Unit] = IO.blocking {
    val timestamp = LocalDateTime.now().format(dtf)
    val logLine = s"[$timestamp] [$level] $msg"
    this.synchronized {
      val fw = new FileWriter(logFilePath, true)
      try { fw.write(logLine + "\n") } finally { fw.close() }
    }
    if (verbose) println(logLine)
  }
  def info(msg: String): IO[Unit] = log("INFO", msg)
  def error(msg: String): IO[Unit] = log("ERROR", msg)
}

sealed trait Query
case class Term(field: String, value: String) extends Query
case class And(left: Query, right: Query) extends Query
case class Or(left: Query, right: Query) extends Query
case class Not(query: Query) extends Query

class BookQueryParser extends JavaTokenParsers {
  val fields = Set("author", "title", "id", "container", "filename")
  
  def parseQuery(input: String): ParseResult[Query] = parseAll(or, input)
  
  def or: Parser[Query] = chainl1(and, "OR" ^^^ { Or(_, _) })
  def and: Parser[Query] = chainl1(termExpr, "AND" ^^^ { And(_, _) })
  
  def termExpr: Parser[Query] = "NOT" ~> termExpr ^^ { Not(_) } | "(" ~> or <~ ")" | fieldTerm | implicitId | plainTerm
  
  def fieldTerm: Parser[Term] =
    ((ident <~ ":") ~ quotedString) >> {
      case f ~ v if fields.contains(f) => success(Term(s"${f}_exact", v))
      case f ~ _ => failure(s"Unknown field: $f")
    } |
    ((ident <~ ":") ~ fieldValue) >> {
      case f ~ v if fields.contains(f) => success(Term(f, v))
      case f ~ _ => failure(s"Unknown field: $f")
    }

  def fieldValue: Parser[String] = rep1(plainWord) ^^ { words => words.mkString(" ") }
  
  def implicitId: Parser[Term] = """[a-fA-F0-9]{40}""".r ^^ { id => Term("id", id) }

  def plainTerm: Parser[Term] = quotedString ^^ { v => Term("_all_exact", v) } | plainPhrase

  def quotedString: Parser[String] = "\"" ~> """[^"]*""".r <~ "\""
  def plainPhrase: Parser[Term] = rep1(plainWord) ^^ { words => Term("_all", words.mkString(" ")) }
  def reserved: Parser[String] = "AND" | "OR" | "NOT"
  def plainWord: Parser[String] = not(reserved) ~> """[^"():\s]+""".r
}

class DslImpl(logger: SimpleLogger) extends DslTransformerFs2Grpc[IO, Metadata] {
  private val parser = new BookQueryParser()
  override def transform(req: DslRequest, ctx: Metadata): IO[SQuery] = {
    for {
      _ <- logger.info(s"--> REQ: '${req.query}'")
      result <- IO {
        parser.parseQuery(req.query) match {
          case parser.Success(internalAst, _) =>
            val sExp = toSExp(internalAst)
            val protoAst = toProtoAst(internalAst)
            SQuery(data = sExp, isSuccess = true, ast = Some(protoAst))
          case f: parser.NoSuccess =>
            SQuery(isSuccess = false, errorMsg = f.msg)
        }
      }
      astLog = result.ast.map(_.toProtoString.replaceAll("""\s+""", " ")).getOrElse("None")
      _ <- logger.info(s"<-- RES: success=${result.isSuccess} data='${result.data}' ast='$astLog'")
    } yield result
  }

  private def toSExp(q: Query): String = q match {
    case Term(f, v) => s"($f '$v')"
    case And(l, r)  => s"(AND ${toSExp(l)} ${toSExp(r)})"
    case Or(l, r)   => s"(OR ${toSExp(l)} ${toSExp(r)})"
    case Not(q)     => s"(NOT ${toSExp(q)})"
  }

  private def toProtoAst(q: Query): SearchQuery = q match {
    case Term(f, v) =>
      SearchQuery(SearchQuery.Query.Filter(FilterNode(field = f, value = v, operator = 1)))
    case And(l, r) =>
      SearchQuery(SearchQuery.Query.Logical(LogicalNode(op = 1, nodes = Seq(toProtoAst(l), toProtoAst(r)))))
    case Or(l, r) =>
      SearchQuery(SearchQuery.Query.Logical(LogicalNode(op = 2, nodes = Seq(toProtoAst(l), toProtoAst(r)))))
    case Not(sub) =>
      SearchQuery(SearchQuery.Query.Logical(LogicalNode(op = 3, nodes = Seq(toProtoAst(sub)))))
  }
}

object Main extends IOApp {
  case class RuntimeConfig(
    host: String,
    port: Int,
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
    val dslSection = data.get("dsl_scala").asInstanceOf[JMap[String, Any]]
    val host = dslSection.get("host").toString
    val port = dslSection.get("port").toString.toDouble.toInt
    val metricsSection = Option(data.get("metrics")).map(_.asInstanceOf[JMap[String, Any]])
    val metricsServices = metricsSection.flatMap(m => Option(m.get("services")).map(_.asInstanceOf[JMap[String, Any]]))
    val metricsPort = metricsServices.flatMap(s => Option(s.get("dsl_scala")).map(_.toString.toDouble.toInt)).getOrElse(59052)
    val mtlsSection = Option(dslSection.get("mtls")).map(_.asInstanceOf[JMap[String, Any]])
    val tlsEnabled = mtlsSection.flatMap(s => Option(s.get("enabled"))).exists(_.toString.toBoolean)
    val tlsCaFile = mtlsSection.flatMap(s => Option(s.get("ca_file"))).map(_.toString).getOrElse("")
    val tlsCertFile = mtlsSection.flatMap(s => Option(s.get("cert_file"))).map(_.toString).getOrElse("")
    val tlsKeyFile = mtlsSection.flatMap(s => Option(s.get("key_file"))).map(_.toString).getOrElse("")
    input.close()
    RuntimeConfig(host, port, metricsPort, tlsEnabled, tlsCaFile, tlsCertFile, tlsKeyFile)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val isVerbose = args.contains("-v") || args.contains("--verbose")
    val logger = new SimpleLogger(isVerbose)
    val configPath = getConfigPath()

    val program = for {
      _ <- logger.info(s"=== DSL SERVER STARTING ===")
      cfg <- IO(loadConfig(configPath)).handleErrorWith { e =>
        logger.error(s"Config Error: ${e.getMessage}") *> IO.raiseError(e)
      }
      _ <- logger.info(s"Listening on ${cfg.host}:${cfg.port}")
      _ <- if (cfg.tlsEnabled) logger.info("mTLS mode enabled") else logger.info("mTLS mode disabled")
      _ <- IO {
        DefaultExports.initialize()
        new HTTPServer(cfg.metricsPort)
      }
      _ <- logger.info(s"Metrics listening on :${cfg.metricsPort}")
      _ <- DslTransformerFs2Grpc.bindServiceResource[IO](new DslImpl(logger)).flatMap { service =>
        val health = new HealthStatusManager()
        health.setStatus("", ServingStatus.SERVING)
        Resource.make(
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
        )(server =>
          logger.info("=== DSL SERVER STOPPING ===") *>    
          IO(server.shutdown().awaitTermination()).void
        )
      }.use { server =>
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
      }
    } yield ()

    program.as(ExitCode.Success)
  }
}
