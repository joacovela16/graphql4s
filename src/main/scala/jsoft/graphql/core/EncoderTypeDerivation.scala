package jsoft.graphql.core

import io.circe.generic.AutoDerivation
import io.circe.parser._
import jsoft.graphql.model.Encoder
import monix.reactive.Observable

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

trait EncoderTypeDerivation extends AutoDerivation {


  implicit def createEncoder[T](implicit d: io.circe.Decoder[T]): Encoder[T] = (data: String) => {
    decode[T](data) match {
      case Left(value) => Observable.raiseError(value)
      case Right(value) => Observable.pure(value)
    }
  }
}

object EncoderTypeDerivation extends EncoderTypeDerivation
