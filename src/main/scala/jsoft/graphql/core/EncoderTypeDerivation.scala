package jsoft.graphql.core

import com.fasterxml.jackson.databind.ObjectMapper
import jsoft.graphql.model.Encoder
import monix.reactive.Observable

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

trait EncoderTypeDerivation {

  private final val mapper: ObjectMapper = Lib.mapper

  implicit def createEncoder[T](implicit classTag: ClassTag[T]): Encoder[T] = (data: String) => {
    Observable.pure(mapper.readValue(data, classTag.runtimeClass).asInstanceOf[T])
  }
}

object EncoderTypeDerivation extends EncoderTypeDerivation
