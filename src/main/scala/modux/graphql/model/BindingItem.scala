package modux.graphql.model

import modux.graphql.model.graphql.{Scope, Type}

final case class BindingItem(scope: Scope, _type: Type)
