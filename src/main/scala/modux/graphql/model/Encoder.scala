package modux.graphql.model

import monix.reactive.Observable

trait Encoder[T] {
  def apply(data: String): Observable[T]
}

