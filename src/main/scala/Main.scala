//import StructTypeDerivation._

import annotations.GQLDescription
import model.Binding
import monix.reactive.Observable

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends EncoderTypeDerivation with StructTypeDerivation {

  @GQLDescription("testing lala")
  case class CurrentLocation(coord: String) {
  }

  case class Location(x: Int, y: Int, sum: Int => Int = x => x * 2)

  sealed trait Car {
    def model: String
  }

  case class Fiat(model: String, year: Int) extends Car

  case class Query(car: List[Car], name: String, current: CurrentLocation, height: Int => Location, list: List[Location])

  case class Mutation(number: Int)

  def main(args: Array[String]): Unit = {

    val query: Map[String, String] = Map(
      "query" ->
        """|{
           |  __type("CurrentLocation"){
           |    name
           |    description
           |    fields(false){
           |      ofType
           |    }
           |  }
           |  car
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

    val queryInstance: Query = Query(
       List(Fiat("strada", 2020)),
      "joaquin",
      CurrentLocation("mdeo"),
      (x: Int) => Location(x, x + 1),
      List(Location(1, 2), Location(3, 4))
    )

    val mutationInstance: Mutation = Mutation(10)

    val binding: Binding = queryInstance.asQuery + mutationInstance.asMutation

    val intPromise: (Map[String, String], Option[String]) => Observable[String] = GraphQL.interpreter(binding)

    val start: Long = System.currentTimeMillis()
    val r: Observable[String] = intPromise(query, None)

    println(Await.result(r.foldLeftL("")(_ + _).executeAsync.runToFuture, Duration.Inf))
    println((System.currentTimeMillis() - start) * 0.001)

  }
}

