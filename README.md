# GraphQL4S

[![version](https://img.shields.io/badge/version-0.1.0-green.svg)](https://github.com/joacovela16/graphql4s)

Another GraphQL interpreter for Scala based on specification [graphql/draft](http://spec.graphql.org/draft/).

## How use it

```scala

import akka.NotUsed
import akka.stream.scaladsl.Source
import akka.util.ByteString
import jsoft.graphql.core.GraphQL
import jsoft.graphql.core.EncoderTypeDerivation._
import jsoft.graphql.core.StructTypeDerivation._
import jsoft.graphql.model.{Binding, Interpreter}
import monix.reactive.Observable

case class User(name: String, age: Int)

case class Query(addUser: User => Unit, getUsers: () => Source[User, NotUsed])

case class Mutation(updateUser: (String, Int) => User)

val queryInstance: Query = Query(
  addUser = user => {
    println(user)
  },
  getUsers = () => {
    Source(List(User("Tom", 30)))
  }
)

val mutationInstance: Mutation = Mutation(
  updateUser = (name, age) => User(name, age)
)

val binding: Binding = queryInstance.asQuery + mutationInstance.asMutation

val interpreter: Interpreter = GraphQL.interpreter(
  binding,
  enableIntrospection = true,
  enableSchemaValidation = true
)

// https://monix.io/docs/current/reactive/observable.html
val params: Map[String, String ] = Map(
  "query" ->
    s"""
       |{
       |  addUser($$user){
       |    name
       |  }
       |  
       |  getUsers{
       |    age
       |  }
       |}
       |""".stripMargin,
  "user" -> """ {"name": "tom", "age": 20} """
)

val body: Option[String] = None 

val monixObservable: Observable[String] = interpreter(params, body)
// You can get a akka source
val akkaSource: Source[ByteString, NotUsed]  = interpreter.asAkkaSource(params, body)
```

### Result

```json
{
  "addUser": {
    "name": "tom"
  },
  "getUser": [
    {"age": 30}
  ]
}
```

If an error is founded it is report as

```json
{
  "errors": [
    {
      "code": "syntax|duplicated-key|undefined|...",
      "message": "..."
    }
  ]
}
```

## Types supported

* case class, class, case object, sealed trait
* String
* Int
* Float
* Double
* Akka Source[A, B]
* TraversableOnce[T]
* Future[T]
* Function0, Function1, Function2, Function3, Function4
* Dates // TODO