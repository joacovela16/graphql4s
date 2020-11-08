package model.executor

import monix.reactive.Observable

import scala.reflect.ClassTag

trait Executor {

  def as[B <: Executor](implicit ct: ClassTag[B]): Option[B] = this match {
    case x: B if ct.runtimeClass.isInstance(this) => Option(x)
    case _ => None
  }

  def joinFields(other: Executor): Executor = {
    {
      for {
        a <- this.as[IObject]
        b <- other.as[IObject]
      } yield IObject(s"${a.id} :+: ${b.id}", a.fields ++ b.fields)
    }.getOrElse(this)
  }

  def call(args: Seq[String]): Option[Observable[Executor]] = this.as[IFunction].map(x => x.invoke(args))

  def isFunction: Boolean = false
}
