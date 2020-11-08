package model

trait Binding {

  def schema: Seq[BindingItem]

  def executor: Executor

  def getBy(scope: Scope): Seq[BindingItem] = schema.filter(_.scope == scope)

  def ++(other: Binding): Binding = {
    val self: Binding = this
    new Binding {
      override def schema: Seq[BindingItem] = self.schema ++ other.schema

      override def executor: Executor = self.executor.joinFields(other.executor)
    }
  }
}

object Binding {
  def apply[R](instance: R, b: IBuild[R], scopeArg: Scope = QUERY_SCOPE): Binding = {

    val exe: Executor = b(instance)

    new Binding {

      override def schema: Seq[BindingItem] = Seq(BindingItem(scopeArg, b.schema))

      override def executor: Executor = exe
    }
  }
}
