package model

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

final case class InputValue(_type: Type, name: String, description: Option[String] = None, defaultValue: Option[String] = None)

final case class Field(
                        _type: Type,
                        name: String,
                        description: Option[String] = None,
                        args: Seq[InputValue] = Nil,
                        isDeprecated: Boolean = false,
                        deprecationReason: Option[String] = None
                      )

final case class EnumValue(name: String, description: Option[String] = None, isDeprecated: Boolean = false, deprecationReason: Option[String])

trait TBuilder[T] {
  def apply(): Type
}
