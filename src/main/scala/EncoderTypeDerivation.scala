
import io.circe.generic.AutoDerivation
import io.circe.parser._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

trait EncoderTypeDerivation extends AutoDerivation {

  implicit def asOption[T](d: T): Option[T] = Option(d)
  implicit def createEncoder[T](implicit d: io.circe.Decoder[T]): model.Encoder[T] = new model.Encoder[T] {
    override def apply(data: String)(implicit ec: ExecutionContext): Future[T] = {
      decode[T](data) match {
        case Left(value) => Future.failed(value)
        case Right(value) => Future.successful(value)
      }
    }
  }
}

object EncoderTypeDerivation extends EncoderTypeDerivation
