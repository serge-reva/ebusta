package ebusta.dsl

import org.scalatest.funsuite.AnyFunSuite

class BookQueryParserSpec extends AnyFunSuite {
  private val parser = new BookQueryParser()

  private def parseOk(input: String): Query = {
    parser.parseQuery(input) match {
      case parser.Success(q, _) => q
      case f: parser.NoSuccess => fail(s"expected parse success for '$input', got: ${f.msg}")
    }
  }

  test("parses plain multi-word query as _all term") {
    assert(parseOk("стивен кинг") == Term("_all", "стивен кинг"))
  }

  test("parses logical AND with field terms") {
    assert(
      parseOk("author:king AND title:it") ==
        And(Term("author", "king"), Term("title", "it"))
    )
  }

  test("rejects incomplete field expression") {
    assert(parser.parseQuery("author:").isInstanceOf[parser.NoSuccess])
  }
}
