package jsoft.graphql.model.executor

import monix.reactive.Observable

final case class IAsync(value: Observable[Executor]) extends Executor