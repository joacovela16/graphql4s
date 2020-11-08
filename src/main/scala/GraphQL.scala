import Parser._
import defaults.Defaults
import model._
import monix.eval.Task
import monix.reactive.Observable
import sext.SextAnyTreeString

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

object GraphQL {

  private final val EXECUTOR_EMPTY: Observable[Executor] = Observable.empty[Executor]
  private final val MAP_EMPTY: Map[String, Type] = Map.empty
  private type COMPUTE = Observable[String]

  private final val NONE: COMPUTE = Observable.empty[String]

  private final val MULTI_FIELD_CODE: String = "multi-fields"

  private implicit def toOption[A](d: A): Option[A] = Some(d)

  private implicit def asRuntimeException(d: String): RuntimeException = new RuntimeException(d)

  private case class Context(fragments: Map[String, FragmentDef], accessor: Accessor, renderer: model.Renderer)

  private def invokeFunction(i: Executor, f: FunctionExtractor, context: Context): COMPUTE = {

    val argsMat: Seq[String] = f.args.collect {
      case LiteralValue(value, _) => Some(value)
      case ValueRef(id) => context.accessor(id)
    }.flatten

    i.call(argsMat) match {
      case Some(value) =>
        val body: List[Expr] = f.body
        value.flatMap(int => projector(int, body, context))
      case None =>
        context.renderer.onError(model.ERROR, "invoke-error", s"Problem trying to resolve ${f.id}")
        NONE
    }
  }

  private def renderField(fieldName: String, obs: Observable[String], renderer: model.Renderer): Observable[String] = {
    obs
      .isEmpty
      .flatMap { isEmpty =>
        if (isEmpty) {
          NONE
        } else {
          (renderer.onFieldStart(fieldName) +: obs :+ renderer.onFieldEnd(fieldName)).foldLeft("")(_ + _)
        }
      }
  }

  private def projector(value: Executor, expr: Iterable[Expr], context: Context): COMPUTE = {
    val renderer: model.Renderer = context.renderer
    value match {
      case atomic: model.IAtomic =>
        Observable(renderer.onAtomic(atomic))

      case model.IAsync(data) =>
        data.flatMap(x => projector(x, expr, context))

      case model.IArray(data) =>
        data
          .flatMap(x => projector(x, expr, context))
          .intersperse(renderer.itemStart, renderer.itemSeparator, renderer.itemEnd)

      case model.IOption(value) => value.flatMap(x => projector(x, expr, context))

      case obj: model.IObject =>

        if (expr.isEmpty) {

          Observable
            .fromIterable(obj.fields)
            .filterNot { case (_, value) => value.isFunction }
            .flatMap { case (fieldName, value) => renderField(fieldName, projector(value, Nil, context), renderer) }
            .intersperse(renderer.onStartObject, renderer.objectSeparator, renderer.onEndObject)
            .foldLeft("")(_ + _)

        } else {
          Observable
            .fromIterable(expr)
            .flatMapIterable {
              case FragmentRef(id) => context.fragments.get(id).fold(List.empty[Expr])(_.body)
              case x => List(x)
            }
            .flatMap {
              case func: FunctionExtractor =>

                obj
                  .getField(func.id)
                  .fold(NONE)(value => renderField(func.id, invokeFunction(value, func, context), renderer))

              case ObjExtractor(id, body) =>

                obj
                  .getField(id)
                  .fold(NONE)(value => renderField(id, projector(value, body, context), renderer))

              case Alias(id, body) =>

                obj
                  .getField(body.id)
                  .fold(NONE)(value => renderField(id, invokeFunction(value, body, context), renderer))
            }
            .intersperse(renderer.onStartObject, renderer.objectSeparator, renderer.onEndObject)
            .switchIfEmpty(Observable(s"${renderer.onStartObject}${renderer.onEndObject}"))
            .foldLeft("")(_ + _)
        }
    }

  }

  private def extractArgs(args: List[RefVal], context: Context): Seq[String] = {
    args.collect {
      case LiteralValue(value, _) => Some(value)
      case ValueRef(id) => context.accessor(id)
    }.flatten
  }

  private def taskValidator(value: Executor, expr: Iterable[Expr], context: Context): Observable[String] = {

    val rend: model.Renderer = context.renderer
    val validationObs: Observable[String] = validatorImpl(value, expr, context)

    validationObs
      .isEmpty
      .flatMap { isEmpty =>
        if (isEmpty) {
          NONE
        } else {
          validationObs
            .foldLeft("")(_ + _)
            .flatMap { result =>
              Observable(
                rend.onStartObject,
                rend.onFieldStart("errors"),
                rend.itemStart,
                result,
                rend.onFieldEnd("errors"),
                rend.itemEnd,
                rend.onEndObject
              )
            }
        }
      }
  }

  private def validatorImpl(value: Executor, expr: Iterable[Expr], context: Context): Observable[String] = {
    val renderer: model.Renderer = context.renderer
    value match {
      case _: model.IAtomic =>
        if (expr.isEmpty) {
          NONE
        } else {
          Observable.pure(renderer.onError(model.ERROR, "expand", "Can't extract items over atomic element"))
        }
      case model.IAsync(data) => data.flatMap(x => validatorImpl(x, expr, context))
      case model.IArray(data) => data.head.flatMap(v => validatorImpl(v, expr, context))
      case model.IOption(value) => value.flatMap(v => validatorImpl(v, expr, context))
      case obj: model.IObject =>

        if (expr.isEmpty)

          Observable
            .fromIterable(obj.fields)
            .flatMap { case (name, value) =>
              if (value.isFunction) {
                Observable(renderer.onError(model.ERROR, "explicit", s"""Function "$name" must be called explicitly."""))
              } else {
                NONE
              }
            }
            .intersperse(renderer.itemSeparator)

        else {

          val exprFinal: Iterable[Expr] = expr
            .flatMap {
              case FragmentRef(id) => context.fragments.get(id).map(_.body).getOrElse(Nil)
              case _: FragmentDef => Nil
              case obj => List(obj)
            }

          val baseObs: Observable[String] = {
            if (exprFinal.map(_.id).toSet.size == exprFinal.size) {
              NONE
            } else {
              val dupl: String = exprFinal.map(_.id).groupBy(identity).collect { case (x, ys) if ys.size > 1 => s"'$x'" }.mkString(", ")
              Observable(renderer.onError(model.ERROR, "duplicated-key", s"Duplicated key $dupl."))
            }
          }

          {
            baseObs ++ Observable
              .fromIterable(exprFinal)
              .flatMap {
                case FragmentRef(id) =>

                  if (context.fragments.contains(id)) {
                    NONE
                  } else {
                    Observable(renderer.onError(model.ERROR, "undefined", s"""Fragment "$id" does not exists."""))
                  }

                case FunctionExtractor(id, args, body) =>
                  obj.getField(id) match {
                    case Some(value) =>

                      value.call(extractArgs(args, context)) match {
                        case Some(result) =>
                          result
                            .flatMap(v => validatorImpl(v, body, context))
                            .onErrorHandle { e =>
                              renderer.onError(model.ERROR, "function-call", e.getMessage)
                            }
                        case None =>
                          Observable(renderer.onError(model.ERROR, "unprocessable", s"""Field "$id" is not a function."""))
                      }
                    case None =>

                      Observable(renderer.onError(model.ERROR, "undefined", s"""Function "$id" does not exists."""))
                  }

                case ObjExtractor(id, body) =>
                  obj
                    .getField(id)
                    .fold(Observable(renderer.onError(model.ERROR, "undefined", s"""Field "$id" does not exists.""")))(value => validatorImpl(value, body, context))

                case Alias(id, body) =>

                  obj.getField(body.id)
                    .fold(Observable(renderer.onError(model.ERROR, "undefined", s"""Function "$id" does not exists."""))) { value =>

                      value
                        .call(extractArgs(body.args, context))
                        .fold(Observable(renderer.onError(model.ERROR, "unprocessable", s"""Field "$id" is not a function."""))) { result =>
                          result
                            .flatMap(v => validatorImpl(v, body.body, context))
                            .onErrorHandle(e => renderer.onError(model.ERROR, "function-call", e.getMessage))
                        }
                    }
              }
          }.intersperse(renderer.itemSeparator)
        }
    }
  }

  private def eval(value: Executor, accessor: model.Accessor, expr: List[Expr], rend: model.Renderer): Observable[String] = {
    val (fragmentDefInit, others) = expr.partition(_.isInstanceOf[FragmentDef])
    val fragmentDef: Map[String, FragmentDef] = fragmentDefInit.collect { case x: FragmentDef => x }.map(x => x.id -> x).toMap
    val context: Context = Context(fragmentDef, accessor, rend)

    taskValidator(value, others, context)
      .switchIfEmpty(projector(value, others, context))
  }

  private def introspection(queryID: String, t: Type): Seq[(String, Executor)] = {

    def inputValueMapper(data: InputValue): Executor = {
      IObject(
        data.name,
        Seq(
          "type" -> typeMapper(data._type),
          "name" -> IString(data.name),
          "description" -> IOption(data.description.fold(EXECUTOR_EMPTY)(x => Observable.pure(IString(x)))),
          "defaultValue" -> IOption(data.defaultValue.fold(EXECUTOR_EMPTY)(x => Observable.pure(IString(x)))),
        )
      )
    }

    def enumMapper(enumValue: EnumValue): Executor = {
      IObject(
        enumValue.name,
        Seq(
          "name" -> IString(enumValue.name),
          "description" -> IOption(enumValue.description.fold(EXECUTOR_EMPTY)(x => Observable.pure(IString(x)))),
          "isDeprecated" -> IBoolean(enumValue.isDeprecated),
          "deprecationReason" -> IOption(enumValue.deprecationReason.fold(EXECUTOR_EMPTY)(x => Observable.pure(IString(x))))
        )
      )
    }

    def typeMapper(t: Type): Executor = {
      IObject(
        t.name,
        Seq(
          "kind" -> IString(t.kind.toString),
          "name" -> IString(t.name),
          "description" -> IOption(t.description.fold(EXECUTOR_EMPTY)(x => Observable.pure(IString(x)))),
          "fields" -> new IFunction {
            override def argumentsLength: Int = 1

            override def invoke(args: Seq[String]): Observable[Executor] = Observable.pure {
              IArray(Observable.fromIterable(t.fields).map(x => typeMapper(x._type)))
            }
          },
          "interfaces" -> IArray(Observable.fromIterable(t.interfaces).map(typeMapper)),
          "possibleTypes" -> IArray(Observable.fromIterable(t.possibleTypes).map(typeMapper)),
          "enumValues" -> new IFunction {
            override def argumentsLength: Int = 1

            override def invoke(args: Seq[String]): Observable[Executor] = Observable.pure(IArray(Observable.fromIterable(t.enumValues).map(enumMapper)))
          },
          "inputFields" -> IArray(Observable.fromIterable(t.inputFields).map(inputValueMapper)),
          "ofType" -> IOption(t.ofType.fold(EXECUTOR_EMPTY)(x => Observable.pure(typeMapper(x))))
        )
      )
    }

    def collectTypes(t: Type): Map[String, Type] = {

      val recursion: Map[String, Type] = Seq(
        t.fields.map(_._type).foldLeft(MAP_EMPTY) { case (acc, x) => acc ++ collectTypes(x) },
        t.inputFields.map(_._type).foldLeft(MAP_EMPTY) { case (acc, x) => acc ++ collectTypes(x) },
        t.interfaces.foldLeft(MAP_EMPTY) { case (acc, x) => acc ++ collectTypes(x) },
        t.ofType.fold(MAP_EMPTY)(collectTypes),
        t.possibleTypes.foldLeft(MAP_EMPTY) { case (acc, x) => acc ++ collectTypes(x) }
      ).reduce(_ ++ _)

      if ((!recursion.contains(t.name)) && (t.isObject || t.isTrait || t.isScalar)) {
        recursion + (t.name -> t)
      } else {
        recursion
      }
    }

    def resolver(queryID: String, data: Map[String, Type]): Seq[(String, Executor)] = {
      val cache: String => Observable[Executor] = {
        val cache: mutable.Map[String, Executor] = mutable.Map.empty[String, Executor]
        (key: String) => {
          data.get(key) match {
            case Some(value) => Observable(cache.getOrElseUpdate(key, typeMapper(value)))
            case None => Observable.raiseError(new RuntimeException(s"Undefined type '$key'"))
          }
        }
      }

      Seq(
        "__type" -> new IFunction {
          override def argumentsLength: Int = 1

          override def invoke(args: Seq[String]): Observable[Executor] =
            args match {
              case key +: Nil => cache(key)
              case _ => Observable.raiseError(new RuntimeException("Invalid number of arguments"))
            }
        },
        "__schema" -> IObject(
          "__schema",
          Seq(
            "description" -> IOption(data.get(queryID).flatMap(_.description).fold(EXECUTOR_EMPTY)(x => Observable.pure(IString(x)))),
            "types" -> IArray(Observable.fromIterable(data.keys).flatMap(cache)),
            "queryType" -> IOption(cache(queryID)),
            "mutationType" -> IOption(EXECUTOR_EMPTY), // todo
            "subscriptionType" -> IOption(EXECUTOR_EMPTY), // todo
            "directives" -> IArray(EXECUTOR_EMPTY) // todo
          )
        )
      )
    }

    resolver(queryID, collectTypes(t))
  }

  def interpreter(bind: Binding)(implicit link: Link, ec: ExecutionContext, rendererFactory: model.RendererFactory = Defaults.JsonFactory): (Map[String, String], Option[String]) => Observable[String] = {

    val valueSchema: Executor = {
      bind.executor.joinFields(introspection(bind.schema))
      builder(instance) match {
        case IObject(id, fields) =>
          IObject(id, fields ++ introspection(id, builder.schema))
        case x => x
      }
    }

    (queryParams: Map[String, String], body: Option[String]) => {

      Observable.fromTask(Task.fromFuture(link.build(queryParams, body))).flatMap { accessor =>
        accessor.query match {
          case Some(value) =>
            Observable.fromTask(Parser.processor(value)).flatMap { xs =>
              val renderer: model.Renderer = rendererFactory.supply()
              eval(valueSchema, accessor, xs, renderer)
            }
          case None => Observable.raiseError(new RuntimeException("query or mutation required"))
        }
      }
    }
  }

}
