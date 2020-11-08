import annotations.{GQLDescription, GQLUnion}
import magnolia._
import model.executor.{Executor, IArray, IAsync, IFunction, INumber, IObject, IString, InterpreterError}
import model.{graphql, _}
import model.graphql.{ENUM, Field, INPUT_OBJECT, INTERFACE, InputValue, Kind, LIST, MUTATION_SCOPE, OBJECT, QUERY_SCOPE, SCALAR, SUBSCRIPTION_SCOPE, Type, UNION}
import monix.reactive.Observable

import scala.collection.mutable
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

trait StructTypeDerivation {

  type Typeclass[T] = IBuild[T]

  def combine[T](ctx: CaseClass[IBuild, T]): IBuild[T] = {

    new IBuild[T] {
      val schema: Type = Type(
        OBJECT,
        ctx.typeName.short,
        ctx.annotations.collectFirst { case x: GQLDescription => x.value },
        ctx.parameters.map { param =>

          val deprecatedDatum: Option[deprecated] = param.annotations.collectFirst { case x: deprecated => x }

          Field(
            param.typeclass.schema,
            param.label,
            param.annotations.collectFirst { case x: GQLDescription => x.value },
            Nil,
            deprecatedDatum.nonEmpty,
            deprecatedDatum.map(_.toString)
          )
        },
        isCaseObject = ctx.isObject
      )

      override def apply(d: T): Executor = {

        val fields: Seq[(String, Executor)] = ctx.parameters.map { x =>
          val label: String = x.label
          val value: x.PType = x.default.getOrElse(x.dereference(d))
          label -> x.typeclass(value)
        }

        IObject(ctx.typeName.short, fields)
      }
    }
  }

  def dispatch[T](ctx: SealedTrait[IBuild, T]): IBuild[T] = {

    val kind: Kind = {
      if (ctx.subtypes.forall(_.typeclass.schema.isCaseObject)) {
        ENUM
      } else if (ctx.annotations.collectFirst { case _: GQLUnion => true }.isDefined) {

        UNION
      } else {
        INTERFACE
      }
    }

    val _type: Type = graphql.Type(
      kind,
      ctx.typeName.short,
      ctx.annotations.collectFirst { case x: GQLDescription => x.value },
      ctx
        .subtypes
        .map { s => s.typeclass.schema.fields }
        .reduceOption[Seq[Field]] { case (x, y) =>
          val xMap: Map[String, Field] = x.map { z => z.name -> z }.toMap
          y.filter { field => xMap.contains(field.name) }
        }.getOrElse(Nil),
      possibleTypes = ctx.subtypes.map(_.typeclass.schema),
      isTrait = true
    )

    new IBuild[T] {
      override def schema: Type = _type

      override def apply(d: T): Executor = {
        ctx.dispatch(d) { s =>
          s.typeclass(s.cast(d))
        }
      }
    }
  }

  implicit def gen[T]: IBuild[T] = macro Magnolia.gen[T]

  private implicit def asError(msg: String): InterpreterError = InterpreterError(msg)

  implicit val stringEnc: IBuild[String] = new IBuild[String] {
    override def schema: Type = Type(SCALAR, "String")

    override def apply(d: String): Executor = IString(d)
  }

  implicit val intEnc: IBuild[Int] = new IBuild[Int] {
    override def schema: Type = Type(SCALAR, "Int")

    override def apply(d: Int): Executor = INumber(d)
  }
  implicit val doubleEnc: IBuild[Double] = new IBuild[Double] {
    override def schema: Type = Type(SCALAR, "Double")

    override def apply(d: Double): Executor = INumber(d)
  }

  implicit def iterableEnc[A, C[x] <: Iterable[x]](implicit e: IBuild[A]): IBuild[C[A]] = new IBuild[C[A]] {

    private val _type: Type = graphql.Type(
      LIST,
      "List",
      ofType = Some(e.schema)
    )

    override def schema: Type = _type

    override def apply(d: C[A]): Executor = {
      IArray(Observable.fromIterable(d).map(e.apply))
    }
  }

  implicit def futureEnc[T](implicit e: IBuild[T]): IBuild[Future[T]] = new IBuild[Future[T]] {
    val schema: Type = graphql.Type(OBJECT, "Future", ofType = Some(e.schema))

    override def apply(d: Future[T]): Executor = IAsync(Observable.from(d).map(e.apply))
  }

  implicit def f0Enc[OUT](implicit build: IBuild[OUT]): IBuild[() => OUT] = new IBuild[() => OUT] {
    val schema: Type = graphql.Type(INPUT_OBJECT, "INPUT_OBJECT", ofType = Some(build.schema))

    override def apply(d: () => OUT): Executor = {
      new IFunction {

        override def argumentsLength: Int = 0

        override def invoke(args: Seq[String]): Observable[Executor] = args match {
          case Nil => Observable(build(d()))
          case _ => Observable.raiseError(new RuntimeException("Calling error"))
        }
      }
    }
  }

  implicit def f1Enc[A, OUT](implicit build: IBuild[OUT], aBuild: IBuild[A], e: Encoder[A]): IBuild[A => OUT] = {

    new IBuild[A => OUT] {
      val schema: Type = graphql.Type(
        INPUT_OBJECT,
        "INPUT_OBJECT",
        inputFields = mutable.ArrayBuffer(
          InputValue(aBuild.schema, aBuild.schema.name)
        ),
        ofType = Some(build.schema)
      )

      override def apply(d: A => OUT): Executor = new IFunction {

        override def argumentsLength: Int = 1

        override def invoke(args: Seq[String]): Observable[Executor] = args match {
          case x +: Nil =>
            e(x).map(d).map(build(_))
          case _ => Observable.raiseError(new RuntimeException("Calling error"))
        }
      }
    }
  }

  implicit def f2Enc[A, B, OUT](implicit build: IBuild[OUT], ab: IBuild[A], bb: IBuild[B], e1: Encoder[A], e2: Encoder[B]): IBuild[(A, B) => OUT] = {
    new IBuild[(A, B) => OUT] {
      val schema: Type = graphql.Type(
        INPUT_OBJECT,
        "INPUT_OBJECT",
        inputFields = mutable.ArrayBuffer(
          graphql.InputValue(ab.schema, ab.schema.name),
          graphql.InputValue(bb.schema, bb.schema.name)
        ),
        ofType = Some(build.schema)
      )

      override def apply(d: (A, B) => OUT): Executor = new IFunction {
        override def argumentsLength: Int = 2

        override def invoke(args: Seq[String]): Observable[Executor] = args match {
          case x +: y +: Nil => for (r1 <- e1(x); r2 <- e2(y)) yield build(d(r1, r2))
          case _ => Observable.raiseError(new RuntimeException("Calling error"))
        }
      }
    }
  }

  implicit def f3Enc[A, B, C, OUT](implicit build: IBuild[OUT], e1: Encoder[A], e2: Encoder[B], e3: Encoder[C]): IBuild[(A, B, C) => OUT] = {

    new IBuild[(A, B, C) => OUT] {
      val schema: Type = ???

      override def apply(d: (A, B, C) => OUT): Executor = new IFunction {
        override def argumentsLength: Int = 3

        override def invoke(args: Seq[String]): Observable[Executor] = args match {
          case x +: y +: z +: Nil => for (r1 <- e1(x); r2 <- e2(y); r3 <- e3(z)) yield build(d(r1, r2, r3))
          case _ => Observable.raiseError(new RuntimeException("Calling error"))
        }
      }
    }
  }

  implicit def f4Enc[A, B, C, D, OUT](implicit builder: IBuild[OUT], e1: Encoder[A], e2: Encoder[B], e3: Encoder[C], e4: Encoder[D]): IBuild[(A, B, C, D) => OUT] = {
    new IBuild[(A, B, C, D) => OUT] {
      val schema: Type = ???

      override def apply(func: (A, B, C, D) => OUT): Executor = new IFunction {
        override def argumentsLength: Int = 3

        override def invoke(args: Seq[String]): Observable[Executor] = args match {
          case a +: b +: c +: d +: Nil =>
            for (r1 <- e1(a); r2 <- e2(b); r3 <- e3(c); r4 <- e4(d)) yield builder(func(r1, r2, r3, r4))
          case _ => Observable.raiseError(new RuntimeException("Calling error"))
        }
      }
    }
  }

  implicit class BindUtils[T](instance: T) {
    def asQuery(implicit tBuilder: IBuild[T]): Binding = Binding(instance, tBuilder, QUERY_SCOPE)

    def asMutation(implicit tBuilder: IBuild[T]): Binding = Binding(instance, tBuilder, MUTATION_SCOPE)

    def asSubscription(implicit tBuilder: IBuild[T]): Binding = Binding(instance, tBuilder, SUBSCRIPTION_SCOPE)
  }

}

object StructTypeDerivation extends StructTypeDerivation