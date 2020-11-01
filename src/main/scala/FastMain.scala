import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.Failure

object FastMain extends App {

  import scala.language.implicitConversions
  import scala.util.{Success, Try}

  final case class FlowError(message: String) extends RuntimeException(message)

  trait Flow[c, i, o] {
    def apply(ctx: c, in: i): Try[o]
  }

  object Flow {
    def apply[c, i, o](f: i => Try[o]): Flow[c, i, o] = (_, input) => f(input)

    def pure[c, i, o](f: => o): Flow[c, i, o] = (_, _) => Try(f)

    def raiseError[c, i, o](f: => Throwable): Flow[c, i, o] = (_, _) => Failure(f)

    def context[C, IN, OUT](f: C => Flow[C, IN, OUT]): Flow[C, IN, OUT] = (ctx, in) => f(ctx)(ctx, in)

    def in[c, i, o](f: i => Flow[c, i, o]): Flow[c, i, o] = (ctx, in) => f(in)(ctx, in)

    def cond[c, i, o](pred: Flow[c, i, Boolean], result: Flow[c, i, o]): (Flow[c, i, Boolean], Flow[c, i, o]) = (pred, result)
  }

  implicit def asFlow[c, i, o](data: o): Flow[c, i, o] = (_, _) => Try(data)

  implicit def asTry[A](datum: A): Try[A] = Success(datum)

  implicit def asThrowable(message: String): Throwable = FlowError(message)

  def flow[c, i, o](f: i => Flow[c, i, o]): Flow[c, i, o] = (ctx, in) => f(in)(ctx, in)

  implicit class FlowUtils[c, i, o](src: Flow[c, i, o]) {
    def pipe(trg: o => Try[o]): Flow[c, i, o] = (ctx, in) => src(ctx, in).flatMap(trg)

    def split[a](pipes: Flow[c, o, a]*): Flow[c, i, Seq[a]] = {
      (ctx, in) =>
        src(ctx, in).flatMap { out =>
          pipes.foldLeft(Try(Seq.empty[a])) { case (acc, x) =>
            for (tail <- acc; head <- x(ctx, out)) yield tail :+ head
          }
        }
    }

    def should(f: => Flow[c, i, Boolean], errorMessage: String): Flow[c, i, o] = (ctx, in) => {
      if (f(ctx, in).getOrElse(false)) src(ctx, in) else Failure(FlowError(errorMessage))
    }

    def switch[a](default: Flow[c, o, a], cond: (Flow[c, i, Boolean], Flow[c, o, a])*): Flow[c, i, a] = {
      (ctx, in) => {

        val result = cond.find { case (preCond, _) => preCond(ctx, in).getOrElse(false) } match {
          case Some((_, result)) => result
          case None => default
        }

        src(ctx, in).flatMap(out => result(ctx, out))
      }
    }

    def map[a](directive1: => Flow[c, o, a]): Flow[c, i, a] = (ctx, in) => src(ctx, in).flatMap { out => directive1(ctx, out) }
  }

  implicit class PipeParallax[c, i, o, S[k] <: GenTraversableOnce[k]](pipe: Flow[c, i, S[o]]) {
    def join(f: Flow[c, S[o], o]): Flow[c, i, o] = (ctx, in) => pipe(ctx, in).flatMap(xs => f(ctx, xs))

    def pipe[a](f: => Flow[c, o, a])(implicit canBuildFrom: CanBuildFrom[S[o], a, S[a]]): Flow[c, i, S[a]] = (ctx, in) => {
      pipe(ctx, in).flatMap { xs =>
        xs.foldLeft(Try(canBuildFrom())) { case (acc, x) => for (items <- acc; item <- f(ctx, x)) yield items += item }.map(_.result())
      }
    }
  }

  val a: Int = 10
  val spec =
    Flow[String, Int, Int](x => x * 2) // 20
      .pipe(_ + 2) // 22
      .pipe(_ - 2) // 20
      .should(Flow.in(_ < 0), "Should less than 0")
      .map {
        Flow.context { ctx =>
          println(ctx)
          Flow.in { x =>
            println(x)
            x
          } // 20
        }
      }
      .split(Flow.in(_ * 1), Flow.in(_ * 2), Flow.pure(3)) // 20, 40, 3
      .pipe(Flow.in(_ + 5)) // 25, 45, 8
      .join(Flow.in(_.sum)) // 135
      .switch(
        Flow.pure {
          println("default")
          3
        },
        Flow.cond(
          Flow.in(x => x < 2),
          Flow.in { in =>
            println("con1")
            in
          }
        ),
        Flow.cond(
          Flow.in(x => x > 2),
          Flow.in { in =>
            println("con2")
            println(in)
            in
          }
        )
      )

  println(spec("hola", a))

}
