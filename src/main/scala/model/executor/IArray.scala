package model.executor

import monix.reactive.Observable

final case class IArray(value: Observable[Executor]) extends Executor
