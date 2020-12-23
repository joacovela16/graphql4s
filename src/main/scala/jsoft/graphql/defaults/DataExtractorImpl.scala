package jsoft.graphql.defaults

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import jsoft.graphql.core.Lib
import jsoft.graphql.model.{Accessor, DataExtractor}

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._
import scala.util.Try

case object DataExtractorImpl extends DataExtractor {

  override def build(queryParams: Map[String, String], body: Option[String])(implicit executionContext: ExecutionContext): Future[Accessor] = Future {

    val mapper: ObjectMapper = Lib.mapper
    val parsedObj: Option[JsonNode] = body.flatMap(x => Try(mapper.readTree(x)).toOption)
    val vs1: Map[String, JsonNode] = queryParams
      .get("variables")
      .flatMap(x => Try(mapper.readTree(x)).toOption)
      .fold(Map.empty[String, JsonNode]) { obj =>
        obj.fields().asScala.map(x => x.getKey -> x.getValue).toMap
      }

    val variableStore: Map[String, JsonNode] = parsedObj.map(obj => obj.fields().asScala.map(x => x.getKey -> x.getValue).toMap ++ vs1).getOrElse(vs1)
    val maybeQuery: Option[String] = parsedObj.flatMap(x => Option(x.get("query"))).flatMap(x => Option(mapper.writeValueAsString(x)))

    new Accessor {
      override def apply(key: String): Option[String] = {
        variableStore.get(key).map(_.toString())
      }

      override def query: Option[String] = queryParams.get("query").orElse(maybeQuery)
    }
  }
}

