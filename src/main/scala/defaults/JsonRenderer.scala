package defaults

import model.Renderer
import model.executor.{IAtomic, IBoolean, IDouble, IInt, IString}

case object JsonRenderer extends Renderer {

  override def id: String = "application/json"

  override def onAtomic(d: IAtomic): String = d match {
    case IString(value) => s""""$value""""
    case IDouble(value) => value.toString
    case IInt(value) => value.toString
    case IBoolean(value) => value.toString
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
