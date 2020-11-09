package modux.graphql.model.executor

import monix.reactive.Observable

final case class IOption(value: Option[Executor]) extends Executor

