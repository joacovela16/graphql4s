package jsoft.graphql.model

import akka.NotUsed
import akka.stream.scaladsl.Source
import akka.util.ByteString
import monix.reactive.Observable

trait Interpreter {
  def apply(queryParams: Map[String, String], body: Option[String]): Observable[String]

  def asAkkaSource(queryParams: Map[String, String], body: Option[String]): Source[ByteString, NotUsed] = {
    import monix.execution.Scheduler.Implicits.global

    val obs: Observable[String] = apply(queryParams, body)
    Source.fromPublisher(obs.toReactivePublisher).map(ByteString(_))
  }
}

object Interpreter {
  def raiseError(throwable: Throwable): Interpreter = (_: Map[String, String], _: Option[String]) => Observable.raiseError(throwable)
}

