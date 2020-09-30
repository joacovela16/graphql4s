//import StructTypeDerivation._
import model.{Commander, Interpreter}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Main extends EncoderTypeDerivation with StructTypeDerivation{

  case class CurrentLocation(coord: String)
  case class Location(x: Int, y: Int)
  case class Human(name: String, height: CurrentLocation => Location)

  def encoder[T](implicit e: Commander[T]): Commander[T] = e

  def main(args: Array[String]): Unit = {

   /* val e: Commander[Human] = encoder[Human]
    val r: Interpreter = Await.result(
      e.command(Human("joaquin", x => {
        println(x)
        Location(10, 10 + 1)
      })),
      Duration.Inf
    )*/
  }
}

