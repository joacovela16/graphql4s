import Parser.{Alias, Expr, FragmentDef, FragmentRef, FunctionExtractor, LiteralValue, ObjExtractor, ValueRef, start}
import defaults.Defaults
import fastparse.{Parsed, parse}
import model.{Accessor, Commander, Interpreter, Link}
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future}

object GraphQL {

  private final val MULTI_FIELD_CODE: String = "multi-fields"

  private def invokeFunction(i: Interpreter, f: FunctionExtractor, fragments: Map[String, FragmentDef], accessor: Accessor, renderer: model.Renderer): Observable[String] = {

    val argsMat: Seq[String] = f.args.collect {
      case LiteralValue(value, _) => Some(value)
      case ValueRef(id) => accessor(id)
    }.flatten

    if (argsMat.length == f.args.length) {

      i.call(argsMat) match {
        case Some(value) =>
          val body: List[Expr] = f.body
          val obs = Observable.fromTask(value)
          obs.flatMap(int => projector(int, body, accessor, fragments, renderer))
        case None =>

          renderer.onError("invoke-error", s"Problem trying to resolve ${f.id}")
          Observable.empty[String]
      }
    } else {
      renderer.onError("missing-argument", s"Variable missing in ${f.id}")
      Observable.empty[String]
    }
  }

  private def projector(interpreterTask: Interpreter, exprs: List[Expr], accessor: model.Accessor, fragments: Map[String, FragmentDef], renderer: model.Renderer): Observable[String] = interpreterTask match {

    case x: model.IAtomic =>

      Observable(renderer.onAtomic(x))

    case x: model.IArray =>
      x.items.flatMap(int => projector(int, exprs, accessor, fragments, renderer)).intersperse(renderer.itemStart, renderer.itemSeparator, renderer.itemEnd)
    case x: model.IOption =>

      x.value.fold(Observable.empty[String])(v => projector(v, exprs, accessor, fragments, renderer))

    case x: model.IObject =>

      val objectExtractors: List[Expr] = exprs
        .flatMap {
          case FragmentRef(id) =>
            fragments.get(id) match {
              case Some(value) => value.body
              case None => Nil
            }
          case x => List(x)
        }

      if (objectExtractors.map(_.id).toSet.size == objectExtractors.size) {

        if (objectExtractors.isEmpty) {
          Observable.empty
        } else {

          Observable
            .fromIterable(objectExtractors)
            .flatMap {
              case ext: FunctionExtractor =>
                val outName: String = ext.id
                if (ext.isInvalid) {
                  renderer.onError(MULTI_FIELD_CODE, s"""Multiple field in "$outName"""")
                  Observable.empty[String]
                } else {
                  x.getField(outName)
                    .fold(Observable.empty[String]) { inter => invokeFunction(inter, ext, fragments, accessor, renderer).map(r => renderer.onObjectField(outName, r)) }
                }

              case obj: ObjExtractor =>
                val name: String = obj.id

                if (obj.isInvalid) {
                  renderer.onError(MULTI_FIELD_CODE, s"""Multiple field in "$name"""")
                  Observable.empty[String]
                } else {
                  x.getField(name).fold(Observable.empty[String]) { int =>
                    projector(int, obj.body, accessor, fragments, renderer).map(r => renderer.onObjectField(name, r))
                  }
                }
              case Alias(out, functionExt) =>

                x.getField(functionExt.id).fold(Observable.empty[String]) { int =>
                  invokeFunction(int, functionExt, fragments, accessor, renderer).map(r => renderer.onObjectField(out, r))
                }

              case _ =>

                println("upps!")
                Observable.empty[String]
            }
            .intersperse(renderer.onStartObject(x.name), renderer.objectSeparator, renderer.onEndObject(x.name))
            .foldLeft("")(_ + _)
        }
      } else {
        renderer.onError(MULTI_FIELD_CODE, s"""Multiple fields in "${x.name}"""")
        Observable.empty[String]
      }
    case _ => Observable.raiseError(new RuntimeException("Unsupported"))
  }


  def eval(interpreter: Interpreter, accessor: model.Accessor, expr: List[Expr], renderer: model.Renderer)(implicit ec: ExecutionContext): Observable[String] = {

    val (fragmentDefInit, others) = expr.partition(_.isInstanceOf[FragmentDef])
    val fragmentDef: Map[String, FragmentDef] = fragmentDefInit.collect { case x: FragmentDef => x }.map(x => x.id -> x).toMap


    projector(interpreter, others, accessor, fragmentDef, renderer: model.Renderer)
  }


  def buildInterpreter[T](schema: T)(implicit link: Link, ec: ExecutionContext, commander: Commander[T], rendererFactory: model.RendererFactory = Defaults.JsonFactory): Task[(Map[String, String], Option[String]) => Task[String]] = {
    commander.command(schema).map { interpreter =>

      (queryParams: Map[String, String], body: Option[String]) => {

        Task.fromFuture(link.build(queryParams, body)).flatMap { accessor =>

          accessor.query match {
            case Some(value) =>

              Parser.processor(value).flatMap { xs =>
                val renderer: model.Renderer = rendererFactory.supply()

                eval(interpreter, accessor, xs, renderer).foldLeftL("")(_ + _)
              }

            case None => Task.raiseError(new RuntimeException("query or mutation required"))
          }
        }
      }
    }
  }


}
