package jsoft.graphql.model

import jsoft.graphql.model.executor.IAtomic

trait Renderer {
  def id: String

  def onAtomic(d: IAtomic): String

  val itemStart: String

  val itemSeparator: String

  val itemEnd: String

  val onStartObject: String

  val objectSeparator: String

  val onEndObject: String

  def onFieldStart(fieldName: String): String

  def onFieldEnd(fieldName: String): String
}