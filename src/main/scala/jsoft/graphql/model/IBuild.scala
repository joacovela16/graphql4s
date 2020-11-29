package jsoft.graphql.model

import jsoft.graphql.model.executor.Executor
import jsoft.graphql.model.graphql.Type

trait IBuild[T] {
  def schema: Type
  def apply(d: T): Executor
}
