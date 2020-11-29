package jsoft.graphql.model.graphql

import scala.collection.mutable

final case class Type(
                       kind: Kind,
                       name: Option[String] = None,
                       description: Option[String] = None,
                       fields: () => Seq[Field] = () => Nil,
                       interfaces: () => mutable.ArrayBuffer[Type] = () => mutable.ArrayBuffer.empty[Type],
                       possibleTypes: () => Seq[Type] = () => Nil,
                       enumValues: mutable.ArrayBuffer[EnumValue] = mutable.ArrayBuffer.empty[EnumValue],
                       inputFields: () => mutable.ArrayBuffer[InputValue] = () => mutable.ArrayBuffer.empty[InputValue],
                       ofType: () => Option[Type] = () => None,
                       required: Boolean = false,
                       isCaseObject: Boolean = false,
                       isTrait: Boolean = false,
                     ) {
  def isScalar: Boolean = kind == SCALAR

  def isObject: Boolean = kind == OBJECT
}
