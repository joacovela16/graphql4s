package jsoft.graphql.defaults

import jsoft.graphql.model.Renderer
import jsoft.graphql.model.executor._

case object JsonRenderer extends Renderer {

  override def id: String = "application/json"

  override def onAtomic(d: IAtomic): String = d match {
    case IString(value) => addDoubleQ(value)
    case IDouble(value) => value.toString
    case IInt(value) => value.toString
    case IBoolean(value) => value.toString
    case IZonedDateTime(value) => addDoubleQ(value.toString)
    case ILocalTime(localTime) => addDoubleQ(localTime.toString)
    case ILocalDateTime(localDateTime) => addDoubleQ(localDateTime.toString)
  }

  val itemStart: String = "["
  val itemSeparator: String = ","
  val itemEnd: String = "]"
  val onStartObject: String = "{"
  val objectSeparator: String = ","
  val onEndObject: String = "}"

  override def onFieldStart(fieldName: String): String = s""""$fieldName": """

  override def onFieldEnd(fieldName: String): String = ""

  private def addDoubleQ(value: String): String = s""""$value""""
}
