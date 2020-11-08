package model

trait IBuild[T] {
  def schema: Type
  def apply(d: T): Executor
}
