package modux.graphql.model.graphql

import scala.collection.mutable

final case class Type(
                       kind: Kind,
                       name: String,
                       description: Option[String] = None,
                       fields: Seq[Field] = Nil,
                       interfaces: mutable.ArrayBuffer[Type] = mutable.ArrayBuffer.empty[Type],
                       possibleTypes: Seq[Type] = Nil,
                       enumValues: mutable.ArrayBuffer[EnumValue] = mutable.ArrayBuffer.empty[EnumValue],
                       inputFields: mutable.ArrayBuffer[InputValue] = mutable.ArrayBuffer.empty[InputValue],
                       ofType: Option[Type] = None,
                       isCaseObject: Boolean = false,
                       isTrait: Boolean = false,
                     ) {
  def isScalar: Boolean = kind == SCALAR

  def isObject: Boolean = kind == OBJECT
}
