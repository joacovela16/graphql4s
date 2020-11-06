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
    def process(interpreter: Value): Task[String]
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

  trait IBuild[T] {
    val schema: Type
    def apply(d: T): Value
  }

  sealed trait Value extends Serializable {

    def as[B <: Value](implicit ct: ClassTag[B]): Option[B] = this match {
      case x: B if ct.runtimeClass.isInstance(this) => Option(x)
      case _ => None
    }

    def call(args: Seq[String]): Option[Observable[Value]] = this.as[IFunction].map(x => x.invoke(args))

    def isObject: Boolean = false

    def isArray: Boolean = false

    def isString: Boolean = false

    def isNumber: Boolean = false

    def isOption: Boolean = false

    def isFunction: Boolean = false

    def isFail: Boolean = false

    def ignore: Boolean = false
  }

  sealed trait IAtomic extends Value

  final case class IString(value: String) extends IAtomic {
    override def isString: Boolean = true

  }

  final case class INumber(value: Double) extends IAtomic {
    override def isNumber: Boolean = true
  }

  final case class IAsync(data: Observable[Value]) extends Value {
  }

  final case class IArray(data: Observable[Value]) extends Value {
    override def isArray: Boolean = true
  }

  final case class IOption(value: Option[Value]) extends Value {
    override def isOption: Boolean = true
  }


  final case class IObject(name: String, fields: Seq[(String, Value)]) extends Value {
    private lazy val fieldMap: Map[String, Value] = fields.toMap

    def isInvalid: Boolean = false

    def getField(fieldName: String): Option[Value] = fieldMap.get(fieldName)

    override def isObject: Boolean = true
  }

  trait IFunction extends Value {
    override def isFunction: Boolean = true

    def argumentsLength: Int

    def apply(args: Seq[String]): Observable[Value] = {
      if (args.length == argumentsLength) invoke(args) else Observable.raiseError(new RuntimeException("Invalid arguments size"))
    }

    def invoke(args: Seq[String]): Observable[Value]
  }

}
