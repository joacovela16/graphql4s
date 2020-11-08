package defaults

import model.Renderer
import model.executor.{IAtomic, INumber, IString}

case object JsonRenderer extends Renderer {

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
