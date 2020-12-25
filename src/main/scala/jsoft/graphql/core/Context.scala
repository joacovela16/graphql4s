package jsoft.graphql.core

import jsoft.graphql.core.Parser.{Expr, FragmentDef}
import jsoft.graphql.model.{Accessor, Renderer}

final case class Context(
                          expr: List[Expr],
                          fragments: Map[String, FragmentDef],
                          accessor: Accessor,
                          renderer: Renderer
                        )
