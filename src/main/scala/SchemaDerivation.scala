import magnolia._
import model._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

trait SchemaDerivation {

  type Typeclass[T] = SchemaDerive[T]

  def combine[T](ctx: CaseClass[SchemaDerive, T]): SchemaDerive[T] = () => model.SObject(
    ctx.typeName.short,
    ctx.parameters.map(x => (x.label, x.typeclass.get())).toMap
  )

  def dispatch[T](ctx: SealedTrait[SchemaDerive, T]): SchemaDerive[T] = ???

  implicit def genSchema[T]: SchemaDerive[T] = macro Magnolia.gen[T]
  implicit val stringEnc: model.SchemaDerive[String] = () => model.SString
  implicit val intEnc: model.SchemaDerive[Int] = () => model.SNumber
  implicit val doubleEnc: model.SchemaDerive[Double] = () => model.SNumber
  implicit def iterableEnc[A, C[x] <: Iterable[x]](implicit e: SchemaDerive[A]): SchemaDerive[C[A]] = () => model.SWrapper("iterable", e.get())
  implicit def futureEnc[T](implicit e: SchemaDerive[T]): SchemaDerive[Future[T]] = () => model.SWrapper("future", e.get())
  implicit def f1Enc[A, OUT](implicit a: SchemaDerive[A], o: SchemaDerive[OUT]): SchemaDerive[A => OUT] = () => model.SFunction(o.get(), a.get())
}

object SchemaDerivation extends SchemaDerivation