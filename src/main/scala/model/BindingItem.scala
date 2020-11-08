package model

import model.graphql.{Scope, Type}

final case class BindingItem(scope: Scope, _type: Type)
