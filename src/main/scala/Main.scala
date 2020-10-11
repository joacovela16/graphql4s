//import StructTypeDerivation._

import defaults.Defaults._
import monix.eval.Task

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends EncoderTypeDerivation with StructTypeDerivation {


  case class CurrentLocation(coord: String)

  case class Location(x: Int, y: Int, sum: Int => Int = x => x * 2)

  case class Human(name: String, current: CurrentLocation, height: Int => Location, list: List[Location])


  def main(args: Array[String]): Unit = {

    val query: Map[String, String] = Map(
      "query" ->
        """|{
           | name
           | current
           | height(10){
           |  y
           |  ...template
           | }
           | fragment template{
           |  x
           | }
           |}
           |""".stripMargin,
      "variables" ->
        """
          |{
          | "var1": 123
          |}
          |""".stripMargin
    )

    import monix.execution.Scheduler.Implicits.global

    val instance: Human = Human("joaquin", CurrentLocation("mdeo"), x => {
      Location(10, 10 + 1)
    }, List(Location(1, 2), Location(3, 4)))


    val start = System.currentTimeMillis()
    val r: Task[String] = GraphQL.buildInterpreter(instance).flatMap(int => int(query, None))
    println(Await.result(r.executeAsync.runToFuture, Duration.Inf))
    println((System.currentTimeMillis() - start) * 0.001)

  }
}

