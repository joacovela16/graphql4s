import io.circe.Decoder

import scala.concurrent.{ExecutionContext, Future}
import scala.language.{existentials, higherKinds, implicitConversions}
import scala.reflect.ClassTag

package object model {

  trait Accessor {
    def apply(key: String): Option[String]
    def query: Future[String]
  }

  trait Render {
    def process(interpreter: Interpreter): String
  }

  case object JsonRender extends Render {
    override def process(interpreter: Interpreter): String = interpreter match {
      case IString(value) => s""""$value""""
      case INumber(value) => value.toString
      case IArray(items) => s"[${items.map(process).mkString(", ")}]"
      case IOption(items) => items.fold("null")(process)
      case IObject(_, fields) => s"{${fields.map { case (k, v) => s""""$k":${process(v)}""" }.mkString(", ")}}"
      case _ => throw new RuntimeException("Unsupported render type")
    }
  }

  trait Link {
    def build(queryParamVar: Option[String], body: Option[String])(implicit executionContext: ExecutionContext): Future[Accessor]
  }

  trait Encoder[T] {
    def apply(data: String)(implicit ec: ExecutionContext): Future[T]
  }

  final case class InterpreterError(message: String) extends Throwable(message)

  trait Commander[T] {
    def command(d: T)(implicit executionContext: ExecutionContext): Future[Interpreter]
  }

  def createCommander[A](f: A => Interpreter): Commander[A] = new Commander[A] {
    override def command(d: A)(implicit executionContext: ExecutionContext): Future[Interpreter] = Future(f(d))
  }

  def createEncoderAsync[A](f: A => ExecutionContext => Interpreter): Commander[A] = new Commander[A] {
    override def command(d: A)(implicit executionContext: ExecutionContext): Future[Interpreter] = Future(f(d)(executionContext))
  }

  sealed trait Interpreter extends Serializable {
    def as[B <: Interpreter](implicit ct: ClassTag[B]): Future[B] = this match {
      case x: B if ct.runtimeClass.isInstance(this) => Future.successful(x)
      case _ => Future.failed(s"Can convert ${this.getClass.getSimpleName}")
    }

    def asObject[A](f: IObject => Future[A])(implicit ec: ExecutionContext): Future[A] = this.as[IObject].flatMap(f)

    def asArray(implicit ec: ExecutionContext): Future[IArray] = this.as[IArray]

    def call(args: Seq[String])(implicit ec: ExecutionContext): Future[Interpreter] = this.as[IFunction].flatMap(x => x.invoke(args))

    def project(fields: Seq[String])(implicit ec: ExecutionContext): Future[Interpreter] = Future(this)

    def isObject: Boolean = false
    def isArray: Boolean = false
    def isString: Boolean = false
    def isNumber: Boolean = false
    def isOption: Boolean = false
    def isFunction: Boolean = false
  }

  final case class IString(value: String) extends Interpreter {
    override def isString: Boolean = true
  }
  final case class INumber(value: Double) extends Interpreter {
    override def isNumber: Boolean = true
  }
  final case class IArray(items: Iterable[Interpreter]) extends Interpreter {
    override def isArray: Boolean = true

    override def project(fields: Seq[String])(implicit ec: ExecutionContext): Future[Interpreter] = {
      Future.sequence(items.map(x => x.project(fields))).map(xs => IArray(xs))
    }
  }
  final case class IOption(items: Option[Interpreter]) extends Interpreter {
    override def isOption: Boolean = true
  }

  final case class IObject(name: String, fields: Seq[(String, Interpreter)]) extends Interpreter {
    private lazy val fieldMap: Map[String, Interpreter] = fields.toMap

    def field[A](fieldName: String)(f: Interpreter => Future[A])(implicit ec: ExecutionContext): Future[A] = getField(fieldName).flatMap(f)

    def getField(fieldName: String): Future[Interpreter] = fieldMap.get(fieldName) match {
      case Some(value) => Future.successful(value)
      case None => Future.failed(s"Field $fieldName undefined in object $name")
    }

    def get(fieldName: String)(implicit ec: ExecutionContext): Future[(String, Interpreter)] = getField(fieldName).map { x => (fieldName, x) }

    override def project(fields: Seq[String])(implicit ec: ExecutionContext): Future[Interpreter] = Future {
      IObject(name, fields.flatMap(x => for (i <- fieldMap.get(x)) yield (x, i)))
    }

    override def isObject: Boolean = true
  }

  trait IFunction extends Interpreter {
    override def isFunction: Boolean = true
    def argumentsLength: Int
    def apply(args: Seq[String]): Future[Interpreter] = {
      if (args.length == argumentsLength) invoke(args) else Future.failed("Invalid arguments size")
    }
    def invoke(args: Seq[String]): Future[Interpreter]

    override def project(fields: Seq[String])(implicit ec: ExecutionContext): Future[Interpreter] = Future.failed("Project unprocessable in Function")
  }

  private implicit def asError(msg: String): InterpreterError = InterpreterError(msg)
}
