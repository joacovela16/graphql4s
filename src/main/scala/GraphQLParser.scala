import fastparse.MultiLineWhitespace._
import fastparse._
import io.circe.{Json, JsonObject}
import model.{Accessor, Commander, Interpreter, Link, SchemaDerive}
import zio.stream.ZStream

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.implicitConversions

object GraphQLParser extends App {

  sealed trait Expr {
    def id: String
  }
  sealed trait RefVal
  final case class LiteralValue(value: String, isNumber: Boolean) extends RefVal
  final case class ValueRef(id: String) extends RefVal
  final case class FragmentRef(id: String) extends Expr
  final case class FragmentDef(id: String, body: List[Expr]) extends Expr
  final case class FunctionExtractor(id: String, args: List[RefVal], body: List[Expr]) extends Expr {
    lazy val isInvalid: Boolean = body.map(_.id).toSet.size != body.size
  }
  final case class ObjExtractor(id: String, body: List[Expr]) extends Expr {
    lazy val isInvalid: Boolean = body.map(_.id).toSet.size != body.size
  }
  final case class Alias(id: String, body: FunctionExtractor) extends Expr

  def name[_: P]: P[String] = P(CharsWhileIn("a-zA-Z0-9", 1).!)

  def stringQuoted[_: P]: P[String] = P("\"" ~ name ~ "\"")

  def number[_: P]: P[String] = P(((CharIn("1-9") ~ CharsWhileIn("0-9", 0)) ~ ("." ~ CharsWhileIn("0-9", 1)).?).!)

  def fragmentIdExpr[_: P]: P[Expr] = P("..." ~ name).map(FragmentRef)

  def variableRef[_: P]: P[RefVal] = P("$" ~ name).map(ValueRef)

  def fieldExp[_: P]: P[ObjExtractor] = P(name).map(ObjExtractor(_, Nil))

  def args[_: P]: P[Seq[RefVal]] = P((name.map(x => LiteralValue(x, isNumber = false)) | number.map(x => LiteralValue(x, isNumber = true)) | variableRef).rep(sep = ","))

  def aliasExpr[_: P]: P[Expr] = P(name ~ ":" ~ funcExpr).map { case (x, y) => Alias(x, y) }

  def fragmentExpr[_: P]: P[FragmentDef] = P("fragment" ~ name ~ "{" ~ bodyExpr ~ "}").map { case (id, body) => FragmentDef(id, body) }

  def objExpr[_: P]: P[Expr] = P(name ~ "{" ~ bodyExpr ~ "}").map { case (str, body) => ObjExtractor(str, body) }

  def funcExpr[_: P]: P[FunctionExtractor] = P(name ~ "(" ~/ args ~/ ")" ~ ("{" ~ bodyExpr.? ~ "}").?).map { case (n, args, body) => FunctionExtractor(n, args.toList, body.flatten.getOrElse(Nil)) }

  def bodyExpr[_: P]: P[List[Expr]] = P((fragmentExpr | aliasExpr | objExpr | funcExpr | fragmentIdExpr | fieldExp) ~ bodyExpr.?).map { case (expr, maybeExpr) =>
    maybeExpr match {
      case Some(value) => expr +: value
      case None => List(expr)
    }
  }

  def expr[_: P]: P[List[Expr]] = P(bodyExpr)

  def start[_: P]: P[List[Expr]] = P("{" ~ bodyExpr ~ "}")

  def eval(interpreter: Interpreter, accessor: model.Accessor, expr: List[Expr])(implicit ec: ExecutionContext): Future[Interpreter] = {

    val (fragmentDefInit, others) = expr.partition(_.isInstanceOf[FragmentDef])
    val fragmentDef: Map[String, FragmentDef] = fragmentDefInit.collect { case x: FragmentDef => x }.map(x => x.id -> x).toMap

    implicit def asFuture[T](d: T): Future[T] = Future.successful(d)

    implicit def asOption[T](d: T): Option[T] = Some(d)

    def invokeFunction(f: FunctionExtractor, i: Interpreter, fragments: Map[String, FragmentDef], accessor: Accessor): Future[Interpreter] = {
      val argsMat: Seq[String] = f.args.collect {
        case LiteralValue(value, _) => Some(value)
        case ValueRef(id) => accessor(id)
      }.flatten

      if (argsMat.length == f.args.length) {

        i.call(argsMat) match {
          case Some(value) =>
            val body: List[Expr] = f.body
            if (body.isEmpty) {
              value
            } else {
              value.flatMap(i => projector(i, body, accessor, fragments))
            }
          case None => Future.failed(new RuntimeException(s"Problem trying to resolve ${f.id}"))
        }
      } else {
        Future.failed(new RuntimeException(s"Variable missing in ${f.id}"))
      }
    }

    def projector(interpreter: Interpreter, exprs: List[Expr], accessor: model.Accessor, fragments: Map[String, FragmentDef]): Future[Interpreter] =
      interpreter match {
        case x: model.IString => x
        case x: model.INumber => x
        case x: model.IError => x
        case x: model.IArray => Future.sequence(x.items.map(x => projector(x, exprs, accessor, fragments))).map(model.IArray)
        case x: model.IOption => x.value.map(y => projector(y, exprs, accessor, fragments)).getOrElse(x)
        case x: model.IObject =>

          val bodyXs = exprs
            .flatMap {
              case FragmentRef(id) =>
                fragments.get(id) match {
                  case Some(value) =>
                    value.body
                  case None => Nil
                }
              case x => List(x)
            }

          if (bodyXs.map(_.id).toSet .size == bodyXs.size) {

            val xs: Seq[Future[(String, Interpreter)]] = bodyXs
              .flatMap {
                case ext: FunctionExtractor =>
                  val outName: String = ext.id

                  if (ext.isInvalid) {
                    Some(Future.successful((outName, model.IError(s"""Multiple field in "$outName""""))))
                  } else {
                    x.getField(outName).map { interp =>
                      invokeFunction(ext, interp, fragments, accessor).map(y => (outName, y))
                    }
                  }
                case obj: ObjExtractor =>

                  val name: String = obj.id

                  if (obj.isInvalid) {
                    Some(Future.successful((name, model.IError(s"""Multiple field "$name""""))))
                  } else {
                    val body: List[Expr] = obj.body
                    if (body.isEmpty) {
                      x.getField(name).map(x => Future.successful((name, x)))
                    } else {
                      x.getField(name).map { int =>
                        projector(int, body, accessor, fragments).map(r => (name, r))
                      }
                    }
                  }

                case Alias(out, body) =>
                  x.getField(body.id).flatMap(int => invokeFunction(body, int, fragments, accessor).map(i => (out, i)))

                case _ =>
                  println("Uppp!!!")

                  None
              }

            Future.sequence(xs).flatMap { ys =>
              model.IObject(x.name, ys)
            }
          }else{
            Future.successful( model.IError(s"""Multiple fields in "${x.name}""""))
          }
        case _ => Future.failed(new RuntimeException("Unsupported"))
      }

    projector(interpreter, others, accessor, fragmentDef)
  }

  def interpreter[T](schema: T)(implicit link: Link, ec: ExecutionContext, commander: Commander[T], render: model.Render = model.JsonRender): (Map[String, String], Option[String]) => Future[String] = {
    val interpreter: Future[Interpreter] = commander.command(schema)

    (queryParams: Map[String, String], body: Option[String]) => {

      interpreter.flatMap { inter =>

        link.build(queryParams, body).flatMap { accessor =>
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

  case class CurrentLocation(coord: String)
  case class Location(x: Int, y: Int, sum: Int => Int = x => x * 2)
  case class Human(name: String, current: CurrentLocation, height: Int => Location, list: List[Location])

  implicit val link: Link = new Link {

    override def build(queryParams: Map[String, String], body: Option[String])(implicit executionContext: ExecutionContext): Future[Accessor] = Future {
      lazy val parsedObj: Option[JsonObject] = body.flatMap(x => io.circe.parser.parse(x).toOption).flatMap(_.asObject)
      lazy val vs1: Map[String, Json] = queryParams.get("variables").flatMap(x => io.circe.parser.parse(x).toOption).flatMap(_.asObject).fold(Map.empty[String, io.circe.Json]) { obj => obj.toMap }
      lazy val variableStore: Map[String, io.circe.Json] = parsedObj.map(obj => obj.toMap ++ vs1).getOrElse(vs1)
      lazy val maybeQuery: Option[String] = parsedObj.flatMap(x => x("query")).flatMap(_.asString)

      new Accessor {
        override def apply(key: String): Option[String] = variableStore.get(key).map(_.toString())

        override def query: Future[String] = queryParams.get("query").orElse(maybeQuery) match {
          case Some(value) => Future.successful(value)
          case None => Future.failed(new RuntimeException("Query not founded"))
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

  val query: Map[String, String] = Map(
    "query" ->
      """|{
         | height(10){
         |  x
         |  ...template
         |  sum(15)
         |  alias: sum(15)
         |  alias2: sum($var1)
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

  import scala.concurrent.ExecutionContext.Implicits.global


  val int = interpreter(instance)
  val r = Await.result(
    int(query, None),
    Duration.Inf
  )
  println(r)
}
