package model


sealed trait LogLevel

case object ERROR extends LogLevel {
  override def toString: String = "error"
}

case object WARN extends LogLevel {
  override def toString: String = "warn"
}

case object INFO extends LogLevel {
  override def toString: String = "info"
}