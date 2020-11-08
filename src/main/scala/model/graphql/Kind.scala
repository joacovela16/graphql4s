package model.graphql

trait Kind {
  private val asStr: String = {
    val tmp: String = this.getClass.getSimpleName
    tmp.substring(0, tmp.length - 1)
  }

  override def toString: String = asStr
}

case object SCALAR extends Kind

case object OBJECT extends Kind

case object INTERFACE extends Kind

case object UNION extends Kind

case object ENUM extends Kind

case object INPUT_OBJECT extends Kind

case object LIST extends Kind

case object NON_NULL extends Kind