package jsoft.graphql.model.graphql

sealed trait Scope
case object QUERY_SCOPE extends Scope
case object MUTATION_SCOPE extends Scope
case object SUBSCRIPTION_SCOPE extends Scope
