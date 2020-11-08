import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{existentials, higherKinds, implicitConversions}
import scala.reflect.ClassTag

package object model {
  type TargetFormat = String
  type QueryParams = Map[String, String]
  type Body = Option[String]

  sealed trait LogLevel

  case object ERROR extends LogLevel {
    override def toString: String = "error"
  }

  case object WARN extends LogLevel {
    override def toString: String = "warn"
  }

  case object INFO extends LogLevel {
    override def toString: String = "info"
  }

  trait RendererFactory {
    def supply(): Renderer
  }

  trait Accessor {
    def apply(key: String): Option[String]

    def query: Option[String]
  }

  trait Render {
    def process(interpreter: Executor): Task[String]
  }

  trait Renderer {
    def id: String

    def onAtomic(d: IAtomic): String

    val itemStart: String

    val itemSeparator: String

    val itemEnd: String

    val onStartObject: String

    val objectSeparator: String

    val onEndObject: String

    def onFieldStart(fieldName: String): String

    def onFieldEnd(fieldName: String): String

    def onError(level: LogLevel, code: String, message: String): String
  }

  trait Link {
    def build(queryParams: Map[String, String], body: Option[String])(implicit executionContext: ExecutionContext): Future[Accessor]
  }

  trait Encoder[T] {
    def apply(data: String): Observable[T]
  }

  final case class InterpreterError(message: String) extends Throwable(message)

  final case class Issue(code: String, message: String)

  sealed trait Executor {

    def as[B <: Executor](implicit ct: ClassTag[B]): Option[B] = this match {
      case x: B if ct.runtimeClass.isInstance(this) => Option(x)
      case _ => None
    }

    def joinFields(other: Executor) : Executor = {
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

  sealed trait IAtomic extends Executor

  final case class IString(value: String) extends IAtomic

  final case class IBoolean(value: Boolean) extends IAtomic

  final case class INumber(value: Double) extends IAtomic

  final case class IAsync(value: Observable[Executor]) extends Executor

  final case class IArray(value: Observable[Executor]) extends Executor

  final case class IOption(value: Observable[Executor]) extends Executor

  final case class IObject(id: String, fields: Seq[(String, Executor)]) extends Executor {
    private val fieldMap: Map[String, Executor] = fields.toMap

    def getField(fieldName: String): Option[Executor] = fieldMap.get(fieldName)
  }

  trait IFunction extends Executor {
    override def isFunction: Boolean = true

    def argumentsLength: Int

    def apply(args: Seq[String]): Observable[Executor] = {
      if (args.length == argumentsLength) invoke(args) else Observable.raiseError(new RuntimeException("Invalid arguments size"))
    }

    def invoke(args: Seq[String]): Observable[Executor]
  }

}
