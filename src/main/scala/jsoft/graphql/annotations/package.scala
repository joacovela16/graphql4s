package jsoft.graphql

package object annotations {

  class GQLDescription(val value: String) extends annotation.Annotation
  class GQLUnion() extends annotation.Annotation

}
