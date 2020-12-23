package jsoft.graphql.core

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.json.JsonWriteFeature
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import jsoft.graphql.core.Parser.{RefVal, ValueRef}

case object Lib {
  final val mapper: ObjectMapper = JsonMapper.builder()
    .configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true)
    .configure(JsonWriteFeature.QUOTE_FIELD_NAMES, false)
    .build()
    .registerModule(DefaultScalaModule)


  def quote(str:String) :String = s"'$str'"

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
