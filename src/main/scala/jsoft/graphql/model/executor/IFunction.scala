package jsoft.graphql.model.executor

import monix.reactive.Observable

trait IFunction extends Executor {
  override def isFunction: Boolean = true

  def checkArgs: Boolean = true

  def argumentsLength: Int

  def apply(args: Seq[String]): Observable[Executor] = {
    if (checkArgs && args.length == argumentsLength) invoke(args) else Observable.raiseError(new RuntimeException("Invalid arguments size"))
  }

  def invoke(args: Seq[String]): Observable[Executor]
}
