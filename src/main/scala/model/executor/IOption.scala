package model.executor

import monix.reactive.Observable

final case class IOption(value: Observable[Executor]) extends Executor

