import io.circe.Decoder

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{existentials, higherKinds, implicitConversions}
import scala.reflect.ClassTag

package object model {

  zio.stream.Stream

  trait Accessor {
    def apply(key: String): Option[String]
    def query: Future[String]
  }

  trait Render {
    def process(interpreter: Interpreter): String
  }

  case object JsonRender extends Render {
    override def process(interpreter: Interpreter): String = {
      def iterator(interpreter: Interpreter, errors: ArrayBuffer[String]): String = interpreter match {
        case IString(value) => s""""$value""""
        case INumber(value) => value.toString
        case IArray(items) => s"[${items.filterNot(_.ignore).map(process).mkString(", ")}]"
        case IOption(items) => items.fold("null")(process)
        case IObject(_, fields) =>

          val out = fields
            .flatMap { case (str, int) =>
              int match {
                case IError(message) =>
                  errors.append(s"""{ "message": "$message" }""")
                  None
                case x => Some(s""" "$str": ${iterator(x, errors)} """)
              }
            }

          s"{${out.mkString(", ")}}"
        case _ => throw new RuntimeException("Unsupported render type")
      }

      val errors: ArrayBuffer[String] = ArrayBuffer.empty[String]
      val output: String = iterator(interpreter, errors)


      if (errors.isEmpty){
        s"""{"data": $output}"""
      }else{
        s"""{"error": [${errors.mkString(", ")}]}"""
      }

    }
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
    def command(d: T)(implicit executionContext: ExecutionContext): Future[Interpreter]
  }

  def createCommander[A](f: A => Interpreter): Commander[A] = new Commander[A] {
    override def command(d: A)(implicit executionContext: ExecutionContext): Future[Interpreter] = Future(f(d))
  }

  def createEncoderAsync[A](f: A => ExecutionContext => Interpreter): Commander[A] = new Commander[A] {
    override def command(d: A)(implicit executionContext: ExecutionContext): Future[Interpreter] = Future(f(d)(executionContext))
  }

  trait SchemaDerive[T]{
    def get(): Schema
  }
  sealed trait Schema
  final case class SObject(name: String, fields: Map[String, Schema]) extends Schema
  final case class SArray(items: Schema) extends Schema
  final case class SWrapper(kind:String, item: Schema) extends Schema
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

    def call(args: Seq[String])(implicit ec: ExecutionContext): Option[Future[Interpreter]] = this.as[IFunction].map(x => x.invoke(args))

    def project(fields: Seq[String]): Interpreter = this

    def isObject: Boolean = false
    def isArray: Boolean = false
    def isString: Boolean = false
    def isNumber: Boolean = false
    def isOption: Boolean = false
    def isFunction: Boolean = false
    def ignore: Boolean = false
  }

  final case class IError(message: String) extends Interpreter

  final case class IString(value: String) extends Interpreter {
    override def isString: Boolean = true
  }

  final case class INumber(value: Double) extends Interpreter {
    override def isNumber: Boolean = true
  }

  final case class IArray(items: Iterable[Interpreter]) extends Interpreter {
    override def isArray: Boolean = true

    override def project(fields: Seq[String]): Interpreter = IArray(items.map(x => x.project(fields)))
  }

  final case class IOption(value: Option[Interpreter]) extends Interpreter {
    override def isOption: Boolean = true

    override def ignore: Boolean = value.isEmpty
  }

  final case class IObject(name: String, fields: Seq[(String, Interpreter)]) extends Interpreter {
    private val fieldsLength: Int = fields.length
    private lazy val fieldMap: Map[String, Interpreter] = fields.toMap

    private lazy val invalid: Boolean = fieldsLength != fieldMap.size

    def isInvalid: Boolean = invalid

    def getField(fieldName: String): Option[Interpreter] = fieldMap.get(fieldName)

    def getFieldForced(fieldName: String): Interpreter = getField(fieldName) match {
      case Some(value) => value
      case None => IError(s"Undefined field $fieldName")
    }

    override def project(fields: Seq[String]): Interpreter = {
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
  }

  private implicit def asError(msg: String): InterpreterError = InterpreterError(msg)
}
