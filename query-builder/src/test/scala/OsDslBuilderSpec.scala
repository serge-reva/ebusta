package ebusta.querybuilder

import org.scalatest.funsuite.AnyFunSuite
import io.circe.Json

import ebusta.library.v1.common.FilterNode
import ebusta.library.v1.common.LogicalNode
import ebusta.library.v1.common.SearchQuery

class OsDslBuilderSpec extends AnyFunSuite {
  test("builds term query for id filter") {
    val ast = SearchQuery(SearchQuery.Query.Filter(FilterNode(field = "id", value = "abc123")))
    val dsl: Json = OsDslBuilder.toDsl(ast)

    val idValue = dsl.hcursor.downField("term").downField("_id").as[String]
    assert(idValue.contains("abc123"))
  }

  test("builds bool/should query for OR logical node") {
    val left = SearchQuery(SearchQuery.Query.Filter(FilterNode(field = "author", value = "king")))
    val right = SearchQuery(SearchQuery.Query.Filter(FilterNode(field = "title", value = "it")))
    val ast = SearchQuery(
      SearchQuery.Query.Logical(
        LogicalNode(op = 2, nodes = Seq(left, right))
      )
    )

    val dsl: Json = OsDslBuilder.toDsl(ast)
    val should = dsl.hcursor.downField("bool").downField("should").focus
    assert(should.exists(_.isArray))
  }

  test("reuses cached base query for repeated AST") {
    QueryCache.configure(maxSize = 100, ttlSeconds = 60)
    QueryCache.clear()

    val ast = SearchQuery(SearchQuery.Query.Filter(FilterNode(field = "author", value = "king")))

    val (firstJson, firstType) = OsStrategy.build(ast, size = 10, from = 0)
    val (secondJson, secondType) = OsStrategy.build(ast, size = 20, from = 10)
    val (hits, misses, _) = QueryCache.stats

    assert(firstType == secondType)
    assert(firstJson.hcursor.downField("params").downField("query").as[String].contains("king"))
    assert(secondJson.hcursor.downField("params").downField("query").as[String].contains("king"))
    assert(secondJson.hcursor.downField("params").downField("size").as[String].contains("20"))
    assert(secondJson.hcursor.downField("params").downField("from").as[String].contains("10"))
    assert(misses >= 1)
    assert(hits >= 1)
  }
}
