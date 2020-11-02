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

    val fields: Seq[(String, Value)] = ctx.parameters.map { x =>
      val label: String = x.label
      val value: x.PType = x.default.getOrElse(x.dereference(d))
      label -> x.typeclass(value)
    }

    IObject(ctx.typeName.short, fields)
  }

  def dispatch[T](ctx: SealedTrait[IBuild, T]): IBuild[T] = (d: T) => {
    ctx.dispatch(d)(s => s.typeclass(s.cast(d)))
  }

  implicit def gen[T]: IBuild[T] = macro Magnolia.gen[T]

  private implicit def asError(msg: String): InterpreterError = InterpreterError(msg)

  implicit val stringEnc: model.IBuild[String] = (d: String) => IString(d)
  implicit val intEnc: model.IBuild[Int] = (d: Int) => INumber(d)
  implicit val doubleEnc: model.IBuild[Double] = (d: Double) => INumber(d)

  implicit def iterableEnc[A, C[x] <: Iterable[x]](implicit e: IBuild[A]): IBuild[C[A]] = (d: C[A]) => {
    model.IArray(Observable.fromIterable(d.map(e(_))))
  }

  implicit def futureEnc[T](implicit e: IBuild[T]): IBuild[Future[T]] = (d: Future[T]) => {
    model.IAsync(Observable.from(d).map(e(_)))
  }

  implicit def f0Enc[OUT](implicit build: IBuild[OUT]): IBuild[() => OUT] = (d: () => OUT) => {

    new IFunction {

      override def argumentsLength: Int = 0

      override def invoke(args: Seq[String]): Observable[Value] = args match {
        case Nil => Observable(build(d()))
        case _ => Observable.raiseError(new RuntimeException("Calling error"))
      }
    }
  }

  implicit def f1Enc[A, OUT](implicit build: IBuild[OUT], e: Encoder[A]): IBuild[A => OUT] = (d: A => OUT) => {

    new IFunction {

      override def argumentsLength: Int = 1

      override def invoke(args: Seq[String]): Observable[Value] = args match {
        case x +: Nil =>
          e(x).map(d).map(build(_))
        case _ => Observable.raiseError(new RuntimeException("Calling error"))
      }
    }
  }

  implicit def f2Enc[A, B, OUT](implicit build: IBuild[OUT], e1: Encoder[A], e2: Encoder[B]): IBuild[(A, B) => OUT] = (d: (A, B) => OUT) => {
    new IFunction {
      override def argumentsLength: Int = 2

      override def invoke(args: Seq[String]): Observable[Value] = args match {
        case x +: y +: Nil => for (r1 <- e1(x); r2 <- e2(y)) yield build(d(r1, r2))
        case _ => Observable.raiseError(new RuntimeException("Calling error"))
      }
    }
  }

  implicit def f3Enc[A, B, C, OUT](implicit build: IBuild[OUT], e1: Encoder[A], e2: Encoder[B], e3: Encoder[C]): IBuild[(A, B, C) => OUT] = (d: (A, B, C) => OUT) => {
    new IFunction {
      override def argumentsLength: Int = 3

      override def invoke(args: Seq[String]): Observable[Value] = args match {
        case x +: y +: z +: Nil => for (r1 <- e1(x); r2 <- e2(y); r3 <- e3(z)) yield build(d(r1, r2, r3))
        case _ => Observable.raiseError(new RuntimeException("Calling error"))
      }
    }
  }

  implicit def f4Enc[A, B, C, D, OUT](implicit builder: IBuild[OUT], e1: Encoder[A], e2: Encoder[B], e3: Encoder[C], e4: Encoder[D]): IBuild[(A, B, C, D) => OUT] = {
    (func: (A, B, C, D) => OUT) => {
      new IFunction {
        override def argumentsLength: Int = 3

        override def invoke(args: Seq[String]): Observable[Value] = args match {
          case a +: b +: c +: d +: Nil =>
            for (r1 <- e1(a); r2 <- e2(b); r3 <- e3(c); r4 <- e4(d)) yield builder(func(r1, r2, r3, r4))
          case _ => Observable.raiseError(new RuntimeException("Calling error"))
        }
      }
    }
  }

}

object StructTypeDerivation extends StructTypeDerivation