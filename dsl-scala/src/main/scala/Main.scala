package ebusta.dsl

import cats.effect.*
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import fs2.grpc.syntax.all.*
import scala.util.parsing.combinator.*
import io.grpc.Metadata

// Java imports
import java.io.{FileWriter, FileInputStream}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Map as JMap
import org.yaml.snakeyaml.Yaml

// gRPC imports
import ebusta.dsl.v1.dsl.*
import ebusta.library.v1.common.*

// --- ЛОГГЕР ---
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

// --- ЛОГИКА ПАРСИНГА ---
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
  def termExpr: Parser[Query] = "NOT" ~> termExpr ^^ { Not(_) } | "(" ~> or <~ ")" | fieldTerm
  def fieldTerm: Parser[Term] = (ident <~ ":") ~ (quotedString | unquotedString) >> {
    case f ~ v if fields.contains(f) => success(Term(f, v))
    case f ~ _ => failure(s"Unknown field: $f")
  }
  def quotedString: Parser[String] = "\"" ~> """[^"]*""".r <~ "\""
  def unquotedString: Parser[String] = """[^ "()]+""".r
}

// --- СЕРВИС ---
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

// --- ЗАПУСК ---
object Main extends IOApp {
  def getConfigPath(): String = sys.env.getOrElse("EBUSTA_CONFIG", "ebusta.yaml")

  def loadConfig(path: String): (String, Int) = {
    val input = new FileInputStream(path)
    val yaml = new Yaml()
    val data = yaml.load(input).asInstanceOf[JMap[String, Any]]
    val dslSection = data.get("dsl_scala").asInstanceOf[JMap[String, Any]]
    val host = dslSection.get("host").toString
    val port = dslSection.get("port").toString.toDouble.toInt  
    input.close()
    (host, port)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val isVerbose = args.contains("-v") || args.contains("--verbose")
    val logger = new SimpleLogger(isVerbose)
    val configPath = getConfigPath()

    val program = for {
      _ <- logger.info(s"=== DSL SERVER STARTING ===")
      configTuple <- IO(loadConfig(configPath)).handleErrorWith { e =>
        logger.error(s"Config Error: ${e.getMessage}") *> IO.raiseError(e)
      }
      (host, port) = configTuple

      _ <- logger.info(s"Listening on $host:$port")
      _ <- DslTransformerFs2Grpc.bindServiceResource[IO](new DslImpl(logger)).flatMap { service =>
        Resource.make(
          IO(NettyServerBuilder.forPort(port).addService(service).build().start())
        )(server =>  
          logger.info("=== DSL SERVER STOPPING ===") *>  
          IO(server.shutdown().awaitTermination()).void
        )
      }.use(_ => IO.never)
    } yield ()

    program.as(ExitCode.Success)
  }
}
