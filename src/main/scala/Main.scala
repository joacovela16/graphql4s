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
           | list{
           |  x
           |  sum(2)
           | }
           | ...template
           | fragment template{
           |  height(50)
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

    val instance: Human = Human(
      "joaquin",
      CurrentLocation("mdeo"),
      x => Location(x, x + 1),
      List(Location(1, 2), Location(3, 4))
    )


    val intPromise: (Map[String, String], Option[String]) => Task[Option[String]] = GraphQL.buildInterpreter(instance)

    val r: Task[Option[String]] = intPromise(query, None)
    println(Await.result(r.executeAsync.runToFuture, Duration.Inf))


  }
}

