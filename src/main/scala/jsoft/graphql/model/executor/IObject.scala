package jsoft.graphql.model.executor

final case class IObject(id: String, fields: Seq[(String, Executor)]) extends Executor {
  private val fieldMap: Map[String, Executor] = fields.toMap

  def getField(fieldName: String): Option[Executor] = fieldMap.get(fieldName)
}

