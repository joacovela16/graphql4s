package jsoft.graphql.model.graphql

final case class Field(
                        `type`: ()=> Type,
                        name: String,
                        description: Option[String] = None,
                        args: Seq[InputValue] = Nil,
                        isDeprecated: Boolean = false,
                        deprecationReason: Option[String] = None
                      )
