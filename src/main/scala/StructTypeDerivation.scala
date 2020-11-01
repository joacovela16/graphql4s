import magnolia._
import model._
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.Future
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

trait StructTypeDerivation {

  type Typeclass[T] = IBuild[T]

  def combine[T](ctx: CaseClass[IBuild, T]): IBuild[T] = (d: T) => {

    val fields = ctx.parameters.map { x =>
      val label = x.label
      val value = x.default.getOrElse(x.dereference(d))
      label -> x.typeclass.build(value)
    }

    IObject(ctx.typeName.short, fields)
  }

  def dispatch[T](ctx: SealedTrait[IBuild, T]): IBuild[T] = ???

  implicit def gen[T]: IBuild[T] = macro Magnolia.gen[T]


  private implicit def asError(msg: String): InterpreterError = InterpreterError(msg)

  implicit val stringEnc: model.IBuild[String] = (d: String) => {
    IString(d)
  }

  implicit val intEnc: model.IBuild[Int] = (d: Int) => INumber(d)


  implicit val doubleEnc: model.IBuild[Double] = (d: Double) => INumber(d)


  implicit def iterableEnc[A, C[x] <: Iterable[x]](implicit e: IBuild[A]): IBuild[C[A]] = (d: C[A]) => {
    model.IArray(Observable.fromIterable(d.map(e.build)))
  }

  implicit def futureEnc[T](implicit e: IBuild[T]): IBuild[Future[T]] = (d: Future[T]) => {
    model.IAsync(Task.from(d).map(e.build))
  }

  implicit def f1Enc[A, OUT](implicit c: IBuild[OUT], e: Encoder[A]): IBuild[A => OUT] = (d: A => OUT) => {

    new IFunction {

      override def argumentsLength: Int = 1

      override def invoke(args: Seq[String]): Task[Value] = args match {
        case x +: Nil =>
          e(x).map(d).map(c.build)
        case _ => Task.raiseError(new RuntimeException("Calling error"))
      }
    }
  }
}

object StructTypeDerivation extends StructTypeDerivation