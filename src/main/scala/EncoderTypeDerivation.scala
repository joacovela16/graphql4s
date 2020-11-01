
import io.circe.generic.AutoDerivation
import io.circe.parser._
import monix.eval.Task

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

trait EncoderTypeDerivation extends AutoDerivation {

  implicit def asOption[T](d: T): Option[T] = Option(d)

  implicit def createEncoder[T](implicit d: io.circe.Decoder[T]): model.Encoder[T] = (data: String) => {
    decode[T](data) match {
      case Left(value) => Task.raiseError(value)
      case Right(value) => Task.pure(value)
    }
  }
}

object EncoderTypeDerivation extends EncoderTypeDerivation
