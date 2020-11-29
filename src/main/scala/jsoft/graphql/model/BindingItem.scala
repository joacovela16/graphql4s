package jsoft.graphql.model

import jsoft.graphql.model.graphql.{Scope, Type}

final case class BindingItem(scope: Scope, _type: Type)
