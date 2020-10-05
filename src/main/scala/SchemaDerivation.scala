import magnolia._
import model._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

trait StructTypeDerivation {

  type Typeclass[T] = Commander[T]

  def combine[T](ctx: CaseClass[Commander, T]): Commander[T] = new Commander[T] {
    override def command(d: T)(implicit executionContext: ExecutionContext): Future[Interpreter] = {
      Future.sequence(
        ctx.parameters.map(y => y.typeclass.command( y.default.getOrElse( y.dereference(d))).map(x => (y.label, x)))
      ).map(x => IObject(ctx.typeName.short, x))
    }
  }

  def dispatch[T](ctx: SealedTrait[Commander, T]): Commander[T] = ???

  implicit def gen[T]: Commander[T] = macro Magnolia.gen[T]

  private implicit def asError(msg: String): InterpreterError = InterpreterError(msg)

  implicit val stringEnc: model.Commander[String] = createCommander[String](IString)

  implicit val intEnc: model.Commander[Int] = createCommander[Int](INumber(_))

  implicit val doubleEnc: model.Commander[Double] = createCommander[Double](INumber)

  implicit def iterableEnc[A, C[x] <: Iterable[x]](implicit e: Commander[A]): Commander[C[A]] = new Commander[C[A]] {
    override def command(d: C[A])(implicit executionContext: ExecutionContext): Future[Interpreter] = Future.sequence(d.map(e.command)).map(IArray)
  }

  implicit def futureEnc[T](implicit e: Commander[T]): Commander[Future[T]] = {
    new Commander[Future[T]] {
      override def command(d: Future[T])(implicit executionContext: ExecutionContext): Future[Interpreter] = d.flatMap(e.command)
    }
  }

  implicit def f1Enc[A, OUT](implicit c: Commander[OUT], e: Encoder[A]): Commander[A => OUT] = createEncoderAsync[A => OUT] { f =>
    implicit ec =>
      new IFunction {

        override def argumentsLength: Int = 1
        override def invoke(args: Seq[String]): Future[Interpreter] = args match {
          case x +: Nil => e(x).map(f).flatMap(c.command)
          case _ => Future.failed("Calling error")
        }
      }
  }
}

object StructTypeDerivation extends StructTypeDerivation