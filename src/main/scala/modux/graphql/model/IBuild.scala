package modux.graphql.model

import modux.graphql.model.executor.Executor
import modux.graphql.model.graphql.Type

trait IBuild[T] {
  def schema: Type
  def apply(d: T): Executor
}
