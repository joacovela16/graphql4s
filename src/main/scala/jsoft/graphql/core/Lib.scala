package jsoft.graphql.core

import jsoft.graphql.core.Parser.{RefVal, ValueRef}

case object Lib {
  def existsVal(refVal: RefVal, context: Context): Boolean = {
    refVal match {
      case ValueRef(id) => context.accessor(id).isDefined
      case _ => true
    }
  }

  def getVal(refVal: RefVal, context: Context): Option[String] = {
    refVal match {
      case ValueRef(id) => context.accessor(id)
      case Parser.LiteralValue(value, _) => Some(value)
    }
  }
}
