package modux.graphql.model.executor

final case class InterpreterError(message: String) extends Throwable(message)
