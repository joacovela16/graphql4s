package model.graphql

final case class Directive(
                            name: String,
                            description: Option[String] = None,
                            locations: Seq[DirectiveLocation] = Nil,
                            args: Seq[InputValue] = Nil,
                            isRepeatable: Boolean = false
                          )
