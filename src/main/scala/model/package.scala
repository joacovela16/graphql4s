import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{existentials, higherKinds, implicitConversions}
import scala.reflect.ClassTag
import scala.util.Try

package object model {
  type TargetFormat = String
  type QueryParams = Map[String, String]
  type Body = Option[String]
//  type EntryPoint = (TargetFormat, QueryParams, Body) => ???

  trait RendererFactory {
    def supply(): Renderer
  }


  trait Accessor {
    def apply(key: String): Option[String]

    def query: Option[String]
  }

  trait Render {
    def process(interpreter: Interpreter): Task[String]
  }

  trait Renderer {
    def id: String

    def onAtomic(d: IAtomic): String

    def itemStart: String

    def itemSeparator: String

    def itemEnd: String

    def onStartObject(objectName: String): String
    def objectSeparator: String

    def onObjectField(fieldName: String, value: String): String

    def onEndObject(objectName: String): String

    def onError(code: String, message: String): Unit
  }


  trait Link {
    def build(queryParams: Map[String, String], body: Option[String])(implicit executionContext: ExecutionContext): Future[Accessor]
  }

  trait Encoder[T] {
    def apply(data: String)(implicit ec: ExecutionContext): Future[T]
  }

  final case class InterpreterError(message: String) extends Throwable(message)

  final case class Issue(code: String, message: String)

  trait Commander[T] {
    def command(d: T)(implicit executionContext: ExecutionContext): Task[Interpreter]
  }

  def createCommander[A](f: A => Interpreter): Commander[A] = new Commander[A] {
    override def command(d: A)(implicit executionContext: ExecutionContext): Task[Interpreter] = Task.fromTry(Try(f(d)))
  }

  def createEncoderAsync[A](f: A => ExecutionContext => Interpreter): Commander[A] = new Commander[A] {
    override def command(d: A)(implicit executionContext: ExecutionContext): Task[Interpreter] = Task.fromTry(Try(f(d)(executionContext)))
  }

  trait SchemaDerive[T] {
    def get(): Schema
  }

  sealed trait Schema

  final case class SObject(name: String, fields: Map[String, Schema]) extends Schema

  final case class SArray(items: Schema) extends Schema

  final case class SWrapper(kind: String, item: Schema) extends Schema

  final case class SFunction(output: Schema, input: Schema*) extends Schema

  case object SBoolean extends Schema

  case object SString extends Schema

  case object SNumber extends Schema

  case object SOptional extends Schema

  sealed trait Interpreter extends Serializable {

    def as[B <: Interpreter](implicit ct: ClassTag[B]): Option[B] = this match {
      case x: B if ct.runtimeClass.isInstance(this) => Option(x)
      case _ => None
    }

    def asObject[A](f: IObject => A): Option[A] = this.as[IObject].map(f)

    def asArray: Option[IArray] = this.as[IArray]

    def call(args: Seq[String]): Option[Task[Interpreter]] = this.as[IFunction].map(x => x.invoke(args))

    def isObject: Boolean = false

    def isArray: Boolean = false

    def isString: Boolean = false

    def isNumber: Boolean = false

    def isOption: Boolean = false

    def isFunction: Boolean = false

    def isFail: Boolean = false

    def ignore: Boolean = false
  }

  sealed trait IAtomic extends Interpreter

  final case class IError(message: String)

  final case class IFail(code: String, message: String) extends Interpreter {
    override def isFail: Boolean = true
  }

  final case class IString(value: String) extends IAtomic {
    override def isString: Boolean = true
  }

  final case class INumber(value: Double) extends IAtomic {
    override def isNumber: Boolean = true
  }

  final case class IArray(items: Observable[Interpreter]) extends Interpreter {
    override def isArray: Boolean = true
  }

  final case class IOption(value: Option[Interpreter]) extends Interpreter {
    override def isOption: Boolean = true
  }

  final case object IIgnore extends Interpreter {
    override def ignore: Boolean = true
  }

  final case class IField(name: String, interpreter: Interpreter, includedIn: String)

  final case class IObject(name: String, fields: Seq[IField]) extends Interpreter {
    private lazy val fieldMap: Map[String, IField] = fields.map(x => x.name -> x).toMap

    def isInvalid: Boolean = false

    def getField(fieldName: String): Option[Interpreter] = fieldMap.get(fieldName).map(_.interpreter)

    override def isObject: Boolean = true
  }

  trait IFunction extends Interpreter {
    override def isFunction: Boolean = true

    def argumentsLength: Int

    def apply(args: Seq[String]): Task[Interpreter] = {
      if (args.length == argumentsLength) invoke(args) else Task.raiseError(new RuntimeException("Invalid arguments size"))
    }

    def invoke(args: Seq[String]): Task[Interpreter]
  }

  //  private implicit def asError(msg: String): InterpreterError = InterpreterError(msg)
}
