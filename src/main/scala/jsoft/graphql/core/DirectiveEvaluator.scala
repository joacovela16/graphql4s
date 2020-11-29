package jsoft.graphql.core

import jsoft.graphql.core.Parser.DirectiveExpr
import jsoft.graphql.model.graphql.GraphQLConst
import monix.reactive.Observable

case object DirectiveEvaluator {
  def apply(maybeDirective: Option[DirectiveExpr], context: Context)(callback: => Observable[String]): Observable[String] = maybeDirective match {
    case Some(directive) =>

      directive.name match {
        case GraphQLConst.INCLUDE =>
          if (extractIfArg(directive, context).contains("true")) {
            callback
          } else {
            Observable.empty[String]
          }

        case GraphQLConst.SKIP =>

          if (extractIfArg(directive, context).contains("true")) {
            Observable.empty[String]
          } else {
            callback
          }
        case _ => Observable.empty[String]
      }
    case None => callback
  }

  private def extractIfArg(directive: DirectiveExpr, context: Context): Option[String] = {
    for {
      ifCond <- directive.args.headOption.filter(a => a.expression == "if" && Lib.existsVal(a.refVal, context))
      value <- Lib.getVal(ifCond.refVal, context)
    } yield value
  }

}
