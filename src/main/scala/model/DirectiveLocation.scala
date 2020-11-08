package model

trait DirectiveLocation {

}

case object DL_QUERY extends DirectiveLocation
case object DL_MUTATION extends DirectiveLocation
case object DL_SUBSCRIPTION extends DirectiveLocation
case object DL_FIELD extends DirectiveLocation
case object DL_FRAGMENT_DEFINITION extends DirectiveLocation
case object DL_FRAGMENT_SPREAD extends DirectiveLocation
case object DL_INLINE_FRAGMENT extends DirectiveLocation
case object DL_VARIABLE_DEFINITION extends DirectiveLocation
case object DL_SCHEMA extends DirectiveLocation
case object DL_SCALAR extends DirectiveLocation
case object DL_OBJECT extends DirectiveLocation
case object DL_FIELD_DEFINITION extends DirectiveLocation
case object DL_ARGUMENT_DEFINITION extends DirectiveLocation
case object DL_INTERFACE extends DirectiveLocation
case object DL_UNION extends DirectiveLocation
case object DL_ENUM extends DirectiveLocation
case object DL_ENUM_VALUE extends DirectiveLocation
case object DL_INPUT_OBJECT extends DirectiveLocation
case object DL_INPUT_FIELD_DEFINITION extends DirectiveLocation
