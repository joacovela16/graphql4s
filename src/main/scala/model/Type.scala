package model

import scala.collection.mutable

sealed trait Kind

case object SCALAR extends Kind

case object OBJECT extends Kind

case object INTERFACE extends Kind

case object UNION extends Kind

case object ENUM extends Kind

case object INPUT_OBJECT extends Kind

case object LIST extends Kind

case object NON_NULL extends Kind

final case class Type(
                       kind: Kind,
                       name: String,
                       description: Option[String] = None,
                       fields: Seq[Field] = Nil,
                       interfaces: mutable.ArrayBuffer[Type] = mutable.ArrayBuffer.empty[Type],
                       possibleTypes: Seq[Type] = Nil,
                       enumValues: mutable.ArrayBuffer[Type] = mutable.ArrayBuffer.empty[Type],
                       inputFields: mutable.ArrayBuffer[InputValue] = mutable.ArrayBuffer.empty[InputValue],
                       ofType: Option[Type] = None,
                       isObject: Boolean = false
                     )

final case class InputValue(_type: Type, name: Option[String] = None, description: Option[String] = None, defaultValue: Option[String] = None)

final case class Field(
                        _type: Type,
                        name: Option[String] = None,
                        description: Option[String] = None,
                        args: Seq[InputValue] = Nil,
                        isDeprecated: Boolean = false,
                        deprecationReason: Option[String] = None
                      )


trait TBuilder[T] {
  def apply(): Type
}
