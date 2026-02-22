package ebusta.dsl

object ParserSelfTest {
  private val parser = new BookQueryParser()

  private def parseOk(input: String): Query = {
    parser.parseQuery(input) match {
      case parser.Success(q, _) => q
      case f: parser.NoSuccess =>
        throw new AssertionError(s"Expected success for '$input', got: ${f.msg}")
    }
  }

  private def parseFail(input: String): Unit = {
    parser.parseQuery(input) match {
      case _: parser.Success[?] =>
        throw new AssertionError(s"Expected failure for '$input', got success")
      case _: parser.NoSuccess =>
        ()
    }
  }

  private def assertEquals[A](actual: A, expected: A, label: String): Unit = {
    if (actual != expected) {
      throw new AssertionError(s"$label: expected=$expected actual=$actual")
    }
  }

  def main(args: Array[String]): Unit = {
    // Plain multi-word should be accepted as _all and routed to template path later.
    assertEquals(parseOk("стивен кинг"), Term("_all", "стивен кинг"), "plain multi-word")
    assertEquals(parseOk("stephen king"), Term("_all", "stephen king"), "plain latin multi-word")

    // Quoted phrases remain supported.
    assertEquals(parseOk("\"стивен кинг\""), Term("_all", "стивен кинг"), "quoted phrase")

    // Existing fielded syntax remains intact.
    assertEquals(parseOk("author:king"), Term("author", "king"), "field term")
    assertEquals(parseOk("title:\"dark tower\""), Term("title", "dark tower"), "field quoted")

    // Logical operators must still work and not be swallowed by plain phrase parser.
    assertEquals(
      parseOk("author:king AND title:it"),
      And(Term("author", "king"), Term("title", "it")),
      "and expression"
    )
    assertEquals(
      parseOk("stephen king OR title:it"),
      Or(Term("_all", "stephen king"), Term("title", "it")),
      "or expression"
    )
    assertEquals(parseOk("NOT title:it"), Not(Term("title", "it")), "not expression")

    // Implicit id must remain stronger than plain phrase.
    assertEquals(
      parseOk("2fb481cc13771f6485091893858808e51a7718ff"),
      Term("id", "2fb481cc13771f6485091893858808e51a7718ff"),
      "implicit id"
    )

    // Invalid expressions should still fail.
    parseFail("author:")
    parseFail("author:king AND")
    parseFail("AND title:it")

    println("[ParserSelfTest] all checks passed")
  }
}
