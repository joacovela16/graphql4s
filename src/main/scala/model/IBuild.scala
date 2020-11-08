package model

import model.executor.Executor
import model.graphql.Type

trait IBuild[T] {
  def schema: Type
  def apply(d: T): Executor
}
