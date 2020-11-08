package defaults

import io.circe.{Json, JsonObject}
import model._

import scala.concurrent.{ExecutionContext, Future}

object Defaults {

  case object JsonFactory extends RendererFactory {
    override def supply(): Renderer = JsonRenderer()
  }

  case class JsonRenderer() extends Renderer {

    override def id: String = "application/json"

    override def onAtomic(d: IAtomic): String = d match {
      case IString(value) => s""""$value""""
      case INumber(value) => s""""$value""""
    }

    val itemStart: String = "["
    val itemSeparator: String = ","
    val itemEnd: String = "]"
    val onStartObject: String = "{"
    val objectSeparator: String = ","
    val onEndObject: String = "}"

    override def onFieldStart(fieldName: String): String = s""""$fieldName": """

    override def onFieldEnd(fieldName: String): String = ""

    override def onError(level: model.LogLevel, code: String, message: String): String = s"""{"message": "$code", "message": "$message"}"""
  }

  final val DEFAULT_LINK: Link = new Link {

    override def build(queryParams: Map[String, String], body: Option[String])(implicit executionContext: ExecutionContext): Future[Accessor] = Future {

      val parsedObj: Option[JsonObject] = body.flatMap(x => io.circe.parser.parse(x).toOption).flatMap(_.asObject)
      val vs1: Map[String, Json] = queryParams.get("variables").flatMap(x => io.circe.parser.parse(x).toOption).flatMap(_.asObject).fold(Map.empty[String, io.circe.Json]) { obj => obj.toMap }
      val variableStore: Map[String, io.circe.Json] = parsedObj.map(obj => obj.toMap ++ vs1).getOrElse(vs1)
      val maybeQuery: Option[String] = parsedObj.flatMap(x => x("query")).flatMap(_.asString)

      new Accessor {
        override def apply(key: String): Option[String] = variableStore.get(key).map(_.toString())

        override def query: Option[String] = queryParams.get("query").orElse(maybeQuery)
      }
    }
  }
}
