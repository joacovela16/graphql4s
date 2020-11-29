package jsoft.graphql.defaults

import jsoft.graphql.lib.ImplicitUtils
import jsoft.graphql.model.graphql.{ENUM, LIST, OBJECT, SCALAR, Type, _}

import scala.collection.mutable

object Introspection extends ImplicitUtils {

  def STRING_TYPE(required: Boolean = false): () => Type = () => Type(SCALAR, "String", required = required)

  def BOOLEAN_TYPE(required: Boolean = false): () => Type = () => Type(SCALAR, "Boolean", required = required)

  def listBuilder(t: Type): Type = Type(LIST, t.name)

  final val __DirectiveLocation = Type(
    ENUM,
    "__DirectiveLocation",
    enumValues = mutable.ArrayBuffer(
      EnumValue("QUERY"),
      EnumValue("MUTATION"),
      EnumValue("SUBSCRIPTION"),
      EnumValue("FIELD"),
      EnumValue("FRAGMENT_DEFINITION"),
      EnumValue("FRAGMENT_SPREAD"),
      EnumValue("INLINE_FRAGMENT"),
      EnumValue("VARIABLE_DEFINITION"),
      EnumValue("SCHEMA"),
      EnumValue("SCALAR"),
      EnumValue("OBJECT"),
      EnumValue("FIELD_DEFINITION"),
      EnumValue("ARGUMENT_DEFINITION"),
      EnumValue("INTERFACE"),
      EnumValue("UNION"),
      EnumValue("ENUM"),
      EnumValue("ENUM_VALUE"),
      EnumValue("INPUT_OBJECT"),
      EnumValue("INPUT_FIELD_DEFINITION"),
    )
  )
  final val __TypeKind = Type(
    ENUM,
    "__TypeKind",
    enumValues = mutable.ArrayBuffer(
      EnumValue("SCALAR"),
      EnumValue("OBJECT"),
      EnumValue("INTERFACE"),
      EnumValue("UNION"),
      EnumValue("ENUM"),
      EnumValue("INPUT_OBJECT"),
      EnumValue("LIST"),
      EnumValue("NON_NULL")
    )
  )

  lazy final val __InputValue = Type(
    OBJECT,
    "__InputValue",
    fields = () => Seq(
      Field(STRING_TYPE(true), "name"),
      Field(STRING_TYPE(), "description"),
      Field(() => __Type, "type"),
      Field(STRING_TYPE(), "defaultValue"),
    )
  )

  lazy final val __EnumValue = Type(
    OBJECT,
    "__EnumValue",
    fields = () => Seq(
      Field(STRING_TYPE(true), "name"),
      Field(STRING_TYPE(), "description"),
      Field(BOOLEAN_TYPE(true), "isDeprecated"),
      Field(STRING_TYPE(), "deprecationReason"),
    )
  )

  final lazy val __Field = Type(
    OBJECT,
    "__Field",
    fields = () => Seq(
      Field(STRING_TYPE(true), "name"),
      Field(STRING_TYPE(), "description"),
      Field(() => Type(LIST, ofType = () => __InputValue, required = true), "args"),
      Field(() => __Type, "type"),
      Field(BOOLEAN_TYPE(true), "isDeprecated"),
      Field(STRING_TYPE(), "deprecationReason"),
    )
  )

  final val __Directive = Type(
    OBJECT,
    "__Directive",
    fields = () => Seq(
      Field(STRING_TYPE(true), "name"),
      Field(STRING_TYPE(), "description"),
      Field(() => Type(LIST, ofType = () => __DirectiveLocation, required = true), "locations"),
      Field(() => Type(LIST, ofType = () => __InputValue, required = true), "args"),
      Field(BOOLEAN_TYPE(true), "isRepeatable")
    )
  )

  final lazy val __Type: Type = Type(
    OBJECT,
    "__Type",
    fields = () => Seq(
      Field(() => __TypeKind.copy(required = true), "kind"),
      Field(STRING_TYPE(), "name"),
      Field(STRING_TYPE(), "description"),
      Field(() => Type(LIST, ofType = () => __Type.copy(required = true)), "fields", args = Seq(InputValue(BOOLEAN_TYPE(), "includeDeprecated", defaultValue = "false"))),
      Field(() => Type(LIST, ofType = () => __Type.copy(required = true)), "interfaces"),
      Field(() => Type(LIST, ofType = () => __Type.copy(required = true)), "possibleTypes"),
      Field(() => Type(LIST, ofType = () => __EnumValue.copy(required = true)), "enumValues", args = Seq(InputValue(BOOLEAN_TYPE(), "includeDeprecated", defaultValue = "false"))),
      Field(() => __InputValue.copy(required = true), "inputFields"),
      Field(() => __Type, "ofType"),
    )
  )

  final val __Schema = Type(
    OBJECT,
    "__Schema",
    None,
    () => Seq(
      Field(STRING_TYPE(), "description"),
      Field(() => Type(LIST, "types"), "types")
    )
  )

  final val ALL: Seq[Type] = Seq(
    __Schema,
    __Type,
    __TypeKind,
    __Field,
    __InputValue,
    __EnumValue,
    __Directive,
    __DirectiveLocation,
  )
}
