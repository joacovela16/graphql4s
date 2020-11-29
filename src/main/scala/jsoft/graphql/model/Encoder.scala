package jsoft.graphql.model

import monix.reactive.Observable

trait Encoder[T] {
  def apply(data: String): Observable[T]
}

