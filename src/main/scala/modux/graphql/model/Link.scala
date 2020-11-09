package modux.graphql.model

import scala.concurrent.{ExecutionContext, Future}

trait Link {
  def build(queryParams: Map[String, String], body: Option[String])(implicit executionContext: ExecutionContext): Future[Accessor]
}
