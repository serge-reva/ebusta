import cats.effect.*
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import fs2.grpc.syntax.all.*
import scala.util.parsing.combinator.*
import io.grpc.Metadata

// Java imports
import java.io.{FileWriter, PrintWriter, File}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Map as JMap
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

import ebusta.dsl.v1.dsl.*

// --- ЛОГГЕР ---
class SimpleLogger(verbose: Boolean, logFilePath: String = "server.log") {
  private val dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")

  // Метод записи
  def log(level: String, msg: String): IO[Unit] = IO.blocking {
    val timestamp = LocalDateTime.now().format(dtf)
    val logLine = s"[$timestamp] [$level] $msg"
    
    // 1. Всегда пишем в файл (append mode)
    // Используем synchronized, чтобы потоки не перемешали строки
    this.synchronized {
      val fw = new FileWriter(logFilePath, true)
      try {
        fw.write(logLine + "\n")
      } finally {
        fw.close()
      }
    }

    // 2. В stdout пишем только если verbose
    if (verbose) {
      println(logLine)
    }
  }

  def info(msg: String): IO[Unit] = log("INFO", msg)
  def error(msg: String): IO[Unit] = log("ERROR", msg)
}

// --- ЛОГИКА DSL ---
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
// Принимаем логгер в конструктор
class DslImpl(logger: SimpleLogger) extends DslTransformerFs2Grpc[IO, Metadata] {
  private val parser = new BookQueryParser()

  override def transform(req: RawInput, ctx: Metadata): IO[SQuery] = {
    for {
      // 1. Логируем входящий запрос
      _ <- logger.info(s"--> REQUEST: query='${req.query}'. Metadata: $ctx")
      
      // 2. Выполняем логику
      result <- IO {
        parser.parseQuery(req.query) match {
          case parser.Success(ast, _) => SQuery(data = toSExp(ast), isSuccess = true)
          case f: parser.NoSuccess    => SQuery(isSuccess = false, errorMsg = f.msg)
        }
      }

      // 3. Логируем ответ
      _ <- logger.info(s"<-- RESPONSE: success=${result.isSuccess}, data='${result.data}', error='${result.errorMsg}'")
    } yield result
  }

  private def toSExp(q: Query): String = q match {
    case Term(f, v) => s"($f '$v')"
    case And(l, r)  => s"(AND ${toSExp(l)} ${toSExp(r)})"
    case Or(l, r)   => s"(OR ${toSExp(l)} ${toSExp(r)})"
    case Not(q)     => s"(NOT ${toSExp(q)})"
  }
}

// --- ЗАПУСК ---
object Main extends IOApp.Simple {

  // Функция для чтения конфига
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

  // Главный метод run
  override def run: IO[Unit] = IO {
    // 0. Разбор аргументов командной строки вручную
    // IOApp.Simple не дает args в run, но мы можем взять системные свойства или сделать хитро.
    // Но проще сделать так:
    val args = sys.props.getOrElse("sun.java.command", "").split(" ")
    val isVerbose = args.contains("-v") || args.contains("--verbose")
    
    // Инициализация логгера
    val logger = new SimpleLogger(isVerbose)
    
    (logger, isVerbose)
  }.flatMap { case (logger, verbose) =>
    
    val configPath = getConfigPath()
    
    // Пытаемся загрузить конфиг
    val configLoad = IO(loadConfig(configPath)).attempt.flatMap {
      case Right((h, p)) => IO.pure((h, p))
      case Left(e) => 
        logger.error(s"Failed to load config from $configPath: ${e.getMessage}") *>
        IO.raiseError(e)
    }

    configLoad.flatMap { case (host, port) =>
      val service = new DslImpl(logger)
      
      DslTransformerFs2Grpc.bindServiceResource[IO](service).flatMap { boundService =>
        Resource.make(
          logger.info(s"=== SERVER STARTING ===") *>
          logger.info(s"Config: $configPath, Verbose: $verbose") *>
          logger.info(s"Listening on $host:$port") *>
          IO(NettyServerBuilder.forPort(port).addService(boundService).build().start())
        )(server => 
          logger.info("=== SERVER STOPPING ===") *> 
          IO(server.shutdown().awaitTermination()).void
        )
      }.use(_ => IO.never)
    }
  }
}
