package jsoft.graphql.core

import fastparse.MultiLineWhitespace._
import fastparse.{CharIn, CharsWhileIn, P, Parsed, parse, _}
import monix.eval.Task

import scala.io.Source

object Parser {

  Source.fromString("asdad")

  sealed trait Expr {
    def id: String
  }

  sealed trait RefVal

  final case class LiteralValue(value: String, isNumber: Boolean) extends RefVal

  final case class ValueRef(id: String) extends RefVal

  final case class FragmentRef(id: String) extends Expr

  final case class FragmentDef(id: String, body: List[Expr]) extends Expr

  final case class DirectiveArg(expression: String, refVal: RefVal)

  final case class DirectiveExpr(name: String, args: List[DirectiveArg])

  final case class FunctionExtractor(id: String, args: List[RefVal], body: List[Expr]) extends Expr {
    lazy val isInvalid: Boolean = body.map(_.id).toSet.size != body.size
  }

  final case class ObjExtractor(id: String, body: List[Expr], directive: Option[DirectiveExpr] = None) extends Expr {
    lazy val isInvalid: Boolean = body.map(_.id).toSet.size != body.size
  }

  final case class Alias(id: String, body: FunctionExtractor) extends Expr

  private def name[_: P]: P[String] = P(CharsWhileIn("_a-zA-Z0-9", 1).!)

  private def stringQuoted[_: P]: P[String] = P( "\"" ~ name ~ "\"" ).map(x=> "\"" + x + "\"")

  private def stringSimple[_: P]: P[String] = P("'" ~ name ~ "'").map(x=> "'" + x + "'")

  private def number[_: P]: P[String] = P(((CharIn("1-9") ~ CharsWhileIn("0-9", 0)) ~ ("." ~ CharsWhileIn("0-9", 1)).?).!)

  private def fragmentIdExpr[_: P]: P[Expr] = P("..." ~ name).map(FragmentRef)

  private def variableRef[_: P]: P[RefVal] = P("$" ~ name).map(ValueRef)

  private def fieldExp[_: P]: P[ObjExtractor] = P(name ~ directive.?).map { case (str, maybeExpr) => ObjExtractor(str, Nil, maybeExpr) }

  private def directiveArgs[_: P]: P[DirectiveArg] = P(name ~ ":" ~ variableRef).map { case (str, refVal) => DirectiveArg(str, refVal) }

  private def directive[_: P]: P[DirectiveExpr] = P("@" ~ name ~ ("(" ~ directiveArgs.rep(0, ",") ~ ")").?)
    .map { case (str, maybeArgs) => DirectiveExpr(str, maybeArgs.map(_.toList).getOrElse(Nil)) }

  private def args[_: P]: P[Seq[RefVal]] = P(
    (
      number.map(x => LiteralValue(x, isNumber = true)) |
        stringQuoted.map(x => LiteralValue(x, isNumber = false)) |
        stringSimple.map(x => LiteralValue(x, isNumber = false)) |
        variableRef
      ).rep(sep = ",")
  )

  private def aliasExpr[_: P]: P[Expr] = P(name ~ ":" ~ funcExpr).map { case (x, y) => Alias(x, y) }

  private def fragmentExpr[_: P]: P[FragmentDef] = P("fragment" ~ name ~ "{" ~ bodyExpr ~ "}").map { case (id, body) => FragmentDef(id, body) }

  private def objExpr[_: P]: P[Expr] = P(name ~ directive.? ~ "{" ~ bodyExpr.? ~ "}").map { case (str, dir, body) => ObjExtractor(str, body.getOrElse(Nil), dir) }

  private def funcExpr[_: P]: P[FunctionExtractor] = P(name ~ "(" ~ args ~ ")" ~ ("{" ~ bodyExpr.? ~ "}").?).map { case (n, args, body) =>
    FunctionExtractor(n, args.toList, body.flatten.getOrElse(Nil))
  }

  private def bodyExpr[_: P]: P[List[Expr]] = P((fragmentExpr | aliasExpr | objExpr | funcExpr | fragmentIdExpr | fieldExp) ~ bodyExpr.?).map { case (expr, maybeExpr) =>
    maybeExpr match {
      case Some(value) => expr +: value
      case None => List(expr)
    }
  }

  private def start[_: P]: P[List[Expr]] = P("{" ~ bodyExpr ~ "}")

  def processor(input: String): Task[List[Expr]] = {
    Task(parse(input, start(_))).flatMap {
      case Parsed.Success(value, _) => Task.pure(value)
      case failure: Parsed.Failure => Task.raiseError(new RuntimeException(failure.msg))
    }
  }

}
