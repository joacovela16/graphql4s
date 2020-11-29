package jsoft.graphql.core

import jsoft.graphql.core.Parser.FragmentDef
import jsoft.graphql.model.{Accessor, Renderer}

final case class Context(fragments: Map[String, FragmentDef], accessor: Accessor, renderer: Renderer)
