import fastparse.MultiLineWhitespace._
import fastparse._
import io.circe.generic.AutoDerivation
import model.{Accessor, Commander, Interpreter, Link}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.implicitConversions

object GraphQLParser {

  sealed trait Expr
  sealed trait RefVal
  final case class LiteralValue(value: String, isNumber: Boolean) extends RefVal
  final case class ValueRef(id: String) extends RefVal
  final case class FragmentRef(id: String) extends Expr
  final case class FragmentDef(id: String, body: List[Expr]) extends Expr
  //  final case class FieldExtractor(id: String) extends Expr
  final case class FunctionExtractor(funcName: String, args: List[RefVal], body: List[Expr]) extends Expr
  final case class ObjExtractor(name: String, body: List[Expr]) extends Expr
  final case class Alias(out: String, body: Expr) extends Expr

  def name[_: P]: P[String] = P((CharIn("a-zA-Z") ~ CharsWhileIn("a-zA-Z0-9", 0)).!)

  def stringQuoted[_: P]: P[String] = P("\"" ~ name ~ "\"")

  def number[_: P]: P[String] = P(((CharIn("1-9") ~ CharsWhileIn("0-9", 0)) ~ ("." ~ CharsWhileIn("0-9", 1)).?).!)

  def fragmentIdExpr[_: P]: P[Expr] = P("..." ~ name).map(FragmentRef)

  def variableRef[_: P]: P[RefVal] = P("$" ~ name).map(ValueRef)

  def fieldExp[_: P]: P[ObjExtractor] = P(name).map(ObjExtractor(_, Nil))

  def args[_: P]: P[Seq[RefVal]] = P((name.map(x => LiteralValue(x, isNumber = false)) | number.map(x => LiteralValue(x, isNumber = true)) | variableRef).rep(sep = ","))

  def aliasExpr[_: P]: P[Expr] = P(name ~ ":" ~/ (funcExpr | objExpr)).map { case (x, y) => Alias(x, y) }

  def fragmentExpr[_: P]: P[FragmentDef] = P("fragment" ~/ name ~/ "{" ~/ bodyExpr ~/ "}").map { case (id, body) => FragmentDef(id, body) }

  def objExpr[_: P]: P[Expr] = P(name ~ "{" ~ bodyExpr ~ "}").map { case (str, body) => ObjExtractor(str, body) }

  def funcExpr[_: P]: P[FunctionExtractor] = P(name ~ "(" ~/ args ~/ ")" ~/ "{" ~/ bodyExpr.? ~/ "}").map { case (n, args, body) => FunctionExtractor(n, args.toList, body.getOrElse(Nil)) }

  def bodyExpr[_: P]: P[List[Expr]] = P(( fragmentExpr | objExpr | funcExpr  | fragmentIdExpr | fieldExp) ~ bodyExpr.?).map { case (expr, maybeExpr) =>
    maybeExpr match {
      case Some(value) => expr +: value
      case None => List(expr)
    }
  }

  def expr[_: P]: P[List[Expr]] = P(bodyExpr)

  def start[_: P]: P[List[Expr]] = P("{" ~ expr ~ "}")

  def eval(interpreter: Interpreter, accessor: model.Accessor, expr: List[Expr])(implicit ec: ExecutionContext): Future[Interpreter] = {

    val (fragmentDefInit, others) = expr.partition(_.isInstanceOf[FragmentDef])
    val fragmentDef: Map[String, FragmentDef] = fragmentDefInit.collect { case x: FragmentDef => x }.map(x => x.id -> x).toMap

    implicit def asFuture[T](d: T): Future[T] = Future.successful(d)

    def projector(interpreter: Interpreter, exprs: List[Expr], accessor: model.Accessor, fragments: Map[String, FragmentDef]): Future[Interpreter] =
      interpreter match {
        case x: model.IString => x
        case x: model.INumber => x
        case x: model.IArray =>
          Future.sequence {
            x.items.map(x => projector(x, exprs, accessor, fragments))
          }.map(model.IArray)
        case x: model.IOption => ???
        case x: model.IObject =>
          //          model.IObject(x.name, )

          val xs: List[Future[(String, Interpreter)]] = exprs.map {
            case FunctionExtractor(funcName, args, body) =>

              x.field(funcName) { field =>

                val argsMat: Seq[String] = args.collect { case LiteralValue(value, _) => value }

                field.call(argsMat).flatMap { result =>
                  if (body.isEmpty) {
                    result
                  } else {
                    projector(result, body, accessor, fragments)
                  }
                }.map(x => (funcName, x))
              }

            case ObjExtractor(name, body) =>

              if (body.isEmpty) {
                x.get(name)
              } else {
                x.field(name) { field =>
                  projector(field, body, accessor, fragments).map(y => (name, y))
                }
              }
            case Alias(out, body) => ???
          }

          Future.sequence(xs).map(ys => model.IObject(x.name, ys))
        case x: model.IFunction => ???
      }

    projector(interpreter, others, accessor, fragmentDef)
  }

  def interpreter[T](schema: T)(implicit link: Link, ec: ExecutionContext, commander: Commander[T], render: model.Render = model.JsonRender) = {
    val interpreter: Future[Interpreter] = commander.command(schema)

    (queryParam: Option[String], body: Option[String]) => {

      interpreter.flatMap { inter =>

        println(inter)
        link.build(queryParam, body).flatMap { accessor =>
          accessor.query.flatMap { q =>

            parse(q, start(_)) match {
              case Parsed.Success(value, _) =>
                println(value)

                eval(inter, accessor, value).map(render.process)

              case failure: Parsed.Failure => Future.failed(new RuntimeException(failure.toString()))
            }
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {

    case class CurrentLocation(coord: String)
    case class Location(x: Int, y: Int, sum: Int => Int = x => x * 2)
    case class Human(name: String, current: CurrentLocation, height: Int => Location, list: List[Location])

    implicit val link: Link = new Link {

      import io.circe.syntax._

      override def build(queryParamVar: Option[String], body: Option[String])(implicit executionContext: ExecutionContext): Future[Accessor] = Future {
        val map: Map[String, io.circe.Json] = body.flatMap(_.asJson.asObject).fold(Map.empty[String, io.circe.Json]) { obj => obj.toIterable.toMap }
        new Accessor {
          override def apply(key: String): Option[String] = map.get(key).map(_.toString())

          override def query: Future[String] = queryParamVar match {
            case Some(value) => Future.successful(value)
            case None => ???
          }
        }
      }
    }

    val instance: Human = Human("joaquin", CurrentLocation("mdeo"), x => {
      println(x)
      Location(10, 10 + 1)
    }, List(Location(1, 2), Location(3, 4)))

    import EncoderTypeDerivation._
    import StructTypeDerivation._

    val query =
      """|{
         | height(10){
         |  x
         |  sum(15){}
         | }
         |}
         |""".stripMargin

    import scala.concurrent.ExecutionContext.Implicits.global
    val int = interpreter(instance)
    val r = Await.result(
      int(query, None),
      Duration.Inf
    )

    println(r)
  }
}
