import java.io.{ByteArrayOutputStream, PrintWriter}
import java.nio.ByteBuffer

import Parser._
import com.sun.org.apache.xalan.internal.xsltc.runtime.output.StringOutputBuffer
import defaults.Defaults
import model.{Accessor, IBuild, Link, Value}
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

object GraphQL {


  private type COMPUTE = Task[Option[String]]

  private final val NONE: COMPUTE = Task.pure(None)

  private final val MULTI_FIELD_CODE: String = "multi-fields"

  private implicit def toOption[A](d: A): Option[A] = Some(d)

  private implicit def asRuntimeException(d: String): RuntimeException = new RuntimeException(d)

  private case class Context(fragments: Map[String, FragmentDef], accessor: Accessor, renderer: model.Renderer)

  private def invokeFunction(i: Value, f: FunctionExtractor, context: Context): COMPUTE = {

    val argsMat: Seq[String] = f.args.collect {
      case LiteralValue(value, _) => Some(value)
      case ValueRef(id) => context.accessor(id)
    }.flatten

    i.call(argsMat) match {
      case Some(value) =>
        val body: List[Expr] = f.body
        value.flatMap(int => projector(int, body, context))
      case None =>
        context.renderer.onError("error", "invoke-error", s"Problem trying to resolve ${f.id}")
        NONE
    }
  }

  private def projector(value: Value, expr: Iterable[Expr], context: Context): COMPUTE = {
    val renderer = context.renderer
    value match {
      case atomic: model.IAtomic => Task.pure(renderer.onAtomic(atomic))
      case model.IAsync(data) => data.flatMap(x => projector(x, expr, context))
      case model.IArray(data) =>
        data
          .mapEval(x => projector(x, expr, context))
          .collect { case Some(value) => value }
          .intersperse(renderer.itemStart, renderer.itemSeparator, renderer.itemEnd)
          .foldLeftL("")(_ + _)
          .map(Some(_))

      case model.IOption(value) => value.fold(NONE)(x => projector(x, expr, context))

      case obj: model.IObject =>

        if (expr.isEmpty) {



          Observable
            .fromIterable(obj.fields)
            .mapEval { case (fieldName, value) =>
              projector(value, Nil, context).map(r => r.map(y => renderer.onObjectField(fieldName, y)))
            }
            .collect { case Some(value) => value }
            .intersperse(renderer.onStartObject, renderer.objectSeparator, renderer.onEndObject)
            .foldLeftL("")(_ + _)
            .map(Some(_))

        } else {
          Observable
            .fromIterable(expr)
            .flatMapIterable {
              case FragmentRef(id) =>
                context.fragments.get(id).fold(List.empty[Expr])(_.body)
              case x => List(x)
            }
            .mapEval {
              case func: FunctionExtractor =>

                obj.getField(func.id)
                  .fold(NONE)(value => invokeFunction(value, func, context))
                  .map(r => r.map(y => renderer.onObjectField(func.id, y)))

              case ObjExtractor(id, body) =>

                obj
                  .getField(id)
                  .fold(NONE)(value => projector(value, body, context))
                  .map(r => r.map(v => renderer.onObjectField(id, v)))

              case Alias(id, body) =>

                obj
                  .getField(body.id)
                  .fold(NONE)(value => invokeFunction(value, body, context))
                  .map(r => r.map(v => renderer.onObjectField(id, v)))
            }
            .collect { case Some(value) => value }
            .intersperse(renderer.onStartObject, renderer.objectSeparator, renderer.onEndObject)
            .foldLeftL("")(_ + _)
            .map(Some(_))
        }

      case _ => NONE
    }

  }


  private def eval(value: Value, accessor: model.Accessor, expr: List[Expr], renderer: model.Renderer): COMPUTE = {
    val (fragmentDefInit, others) = expr.partition(_.isInstanceOf[FragmentDef])
    val fragmentDef: Map[String, FragmentDef] = fragmentDefInit.collect { case x: FragmentDef => x }.map(x => x.id -> x).toMap

    projector(value, others, Context(fragmentDef, accessor, renderer))
  }


  def buildInterpreter[T](schema: T)
                         (implicit link: Link,
                          ec: ExecutionContext,
                          commander: IBuild[T],
                          rendererFactory: model.RendererFactory = Defaults.JsonFactory
                         ): (Map[String, String], Option[String]) => Task[Option[String]] = {

    val fields = commander.build(schema)

    (queryParams: Map[String, String], body: Option[String]) => {

      Task.fromFuture(link.build(queryParams, body)).flatMap { accessor =>

        accessor.query match {
          case Some(value) =>

            Parser.processor(value).flatMap { xs =>
              println(xs)
              val renderer: model.Renderer = rendererFactory.supply()
              val start = System.currentTimeMillis()
              eval(fields, accessor, xs, renderer).map { r =>
                println((System.currentTimeMillis() - start) * 0.001)
                r
              }
            }

          case None => Task.raiseError(new RuntimeException("query or mutation required"))
        }
      }
    }

  }


}
