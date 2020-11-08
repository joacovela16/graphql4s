package model

final case class Directive(
                            name: String,
                            description: Option[String] = None,
                            locations: Seq[DirectiveLocation] = Nil,
                            args: Seq[InputValue] = Nil,
                            isRepeatable: Boolean = false
                          )
