package jsoft.graphql.model

import scala.concurrent.{ExecutionContext, Future}

trait DataExtractor {
  def build(queryParams: Map[String, String], body: Option[String])(implicit executionContext: ExecutionContext): Future[Accessor]
}
