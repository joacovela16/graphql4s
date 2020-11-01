package defaults

import io.circe.{Json, JsonObject}
import model.{Accessor, IAtomic, INumber, IString, Link, Renderer, RendererFactory}

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

    override def itemStart: String = "["

    override def itemSeparator: String = ","

    override def itemEnd: String = "]"

    override def onStartObject: String = "{"

    override def onObjectField(fieldName: String, value: String): String = {
      s""""$fieldName": $value"""
    }

    override def onEndObject: String = "}"

    override def onError(level: String, code: String, message: String): Unit = {
      println(s"$level - $code - $message")
    }

    override def objectSeparator: String = ","
  }

  implicit val link: Link = new Link {

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
