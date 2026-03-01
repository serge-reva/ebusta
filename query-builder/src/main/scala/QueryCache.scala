package ebusta.querybuilder

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

import java.util.concurrent.TimeUnit

import io.circe.Json
import ebusta.qb.v1.query_builder.QueryType

object QueryCache {
  private val defaultMaxSize: Long = 1000L
  private val defaultTtlSeconds: Long = 60L

  @volatile private var cache: Cache[String, (Json, QueryType)] =
    buildCache(defaultMaxSize, defaultTtlSeconds)

  private def buildCache(maxSize: Long, ttlSeconds: Long): Cache[String, (Json, QueryType)] =
    Caffeine
      .newBuilder()
      .maximumSize(math.max(1L, maxSize))
      .expireAfterWrite(math.max(1L, ttlSeconds), TimeUnit.SECONDS)
      .recordStats()
      .build[String, (Json, QueryType)]()

  def configure(maxSize: Long, ttlSeconds: Long): Unit = synchronized {
    cache = buildCache(maxSize, ttlSeconds)
  }

  def getOrCompute(key: String)(compute: => (Json, QueryType)): (Json, QueryType) = {
    val cached = cache.getIfPresent(key)
    if (cached != null) {
      cached
    } else {
      val value = compute
      cache.put(key, value)
      value
    }
  }

  def clear(): Unit = cache.invalidateAll()

  def stats: (Long, Long, Long) = {
    val s = cache.stats()
    (s.hitCount(), s.missCount(), cache.estimatedSize())
  }
}
