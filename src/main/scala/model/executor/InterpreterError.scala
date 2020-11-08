package model.executor

final case class InterpreterError(message: String) extends Throwable(message)
