package jsoft.graphql.core

import jsoft.graphql.core.Parser.{Expr, FragmentDef, FunctionExtractor, RefVal, _}
import jsoft.graphql.defaults.{Introspection, JsonRenderer, LinkImpl}
import jsoft.graphql.model.executor._
import jsoft.graphql.model.graphql._
import jsoft.graphql.model.{Accessor, Binding, Link, Renderer}
import monix.eval.Task
import monix.reactive.Observable
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.language.{existentials, implicitConversions}
import scala.util.{Failure, Success, Try}

object GraphQL {

  private lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
  private final val EXECUTOR_EMPTY: Observable[Executor] = Observable.empty[Executor]
  private final val MAP_EMPTY: Map[String, Type] = Map.empty
  private final val NONE: COMPUTE = Observable.empty[String]

  private type COMPUTE = Observable[String]

  private implicit def toOption[A](d: A): Option[A] = Some(d)

  private implicit def asRuntimeException(d: String): RuntimeException = new RuntimeException(d)

  private def invokeFunction(i: Executor, f: FunctionExtractor, context: Context): COMPUTE = {

    val argsMat: Seq[String] = f.args.collect {
      case LiteralValue(value, _) => Some(value)
      case ValueRef(id) => context.accessor(id)
    }.flatten

    i.call(argsMat) match {
      case Some(value) =>
        val body: Seq[Expr] = f.body
        value.flatMap(int => projector(int, body, context))
      case None =>
        logger.error(onError("invoke-error", s"Problem trying to resolve ${f.id}", context.renderer))
        NONE
    }
  }

  private def renderField(fieldName: String, obs: Observable[String], renderer: Renderer): Observable[String] = {
    obs
      .isEmpty
      .flatMap { isEmpty =>
        if (isEmpty) {
          NONE
        } else {
          renderer.onFieldStart(fieldName) +: obs :+ (renderer.onFieldEnd(fieldName) + renderer.itemSeparator)
        }
      }
  }

  private def projector(value: Executor, expr: Iterable[Expr], context: Context): COMPUTE = {
    val renderer: Renderer = context.renderer
    value match {
      case atomic: IAtomic => Observable(renderer.onAtomic(atomic))
      case x: IFunction if x.argumentsLength == 0 => invokeFunction(x, FunctionExtractor("tmp", Nil, expr.toList), context)
      case IAsync(data) => data.flatMap(x => projector(x, expr, context))
      case IArray(data) =>

        val middle: Observable[String] = data.flatMap(x => projector(x, expr, context) :+ renderer.itemSeparator).dropLast(1)
        renderer.itemStart +: middle :+ renderer.itemEnd

      case IOption(value) => value.fold(NONE)(x => projector(x, expr, context))

      case obj: IObject =>

        if (expr.isEmpty) {
          val middle: Observable[String] = Observable
            .fromIterable(obj.fields)
            .filterNot { case (_, value) => value.isFunction }
            .flatMap { case (fieldName, value) => renderField(fieldName, projector(value, Nil, context), renderer) }
            .dropLast(1)

          renderer.onStartObject +: middle :+ renderer.onEndObject

        } else {
          val middle: Observable[String] = Observable
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

              case ObjExtractor(id, body, directive) =>

                DirectiveEvaluator(directive, context) {
                  obj
                    .getField(id)
                    .fold(NONE)(value => renderField(id, projector(value, body, context), renderer))
                }

              case Alias(id, body) =>

                obj
                  .getField(body.id)
                  .fold(NONE)(value => renderField(id, invokeFunction(value, body, context), renderer))
            }.dropLast(1)

          renderer.onStartObject +: middle :+ renderer.onEndObject
        }
      case _ => NONE
    }

  }

  private def extractArgs(args: List[RefVal], context: Context): Seq[String] = {
    args.collect {
      case LiteralValue(value, _) => Some(value)
      case ValueRef(id) => context.accessor(id)
    }.flatten
  }

  private def taskValidator(value: Executor, expr: Iterable[Expr], context: Context): Observable[String] = {

    val rend: Renderer = context.renderer
    val validationObs: Observable[String] = validatorImpl(value, expr, context)

    validationObs
      .isEmpty
      .flatMap { isEmpty =>
        if (isEmpty) {
          NONE
        } else {

          val start: Observable[String] = Observable(rend.onStartObject, rend.onFieldStart("errors"), rend.itemStart)
          val end: Observable[String] = Observable(rend.onFieldEnd("errors"), rend.itemEnd, rend.onEndObject)

          start ++ validationObs.intersperse(context.renderer.itemSeparator) ++ end
        }
      }
  }

  private def onError(code: String, message: String, renderer: Renderer): String = {
    val codeStr = s"""${renderer.onFieldStart("code")}"$code"${renderer.onFieldEnd("code")}"""
    val msgStr = s"""${renderer.onFieldStart("message")}"$message"${renderer.onFieldEnd("message")}"""
    s"${renderer.onStartObject}$codeStr, $msgStr${renderer.onEndObject}"
  }

  private def validateDirective(maybeDirective: Option[DirectiveExpr], context: Context)(callback: => Observable[String]): Observable[String] = {
    val renderer: Renderer = context.renderer

    maybeDirective match {
      case Some(directive) =>

        directive.name match {
          case GraphQLConst.INCLUDE | GraphQLConst.SKIP =>
            val argsTmp: List[DirectiveArg] = directive.args

            if (argsTmp.length != 1 || !argsTmp.exists(_.expression == "if")) {
              Observable.pure(
                onError("directive-argument", s"Directive '${directive.name}' must include only 'if' argument.", renderer)
              )
            } else if (!argsTmp.exists(a => Lib.existsVal(a.refVal, context))) {
              Observable.pure(
                onError("directive-argument", "Check if variable are defined", renderer)
              )
            } else {
              callback
            }
          case x =>
            Observable.pure(onError("directive-error", s"Directive '$x' is not supported", renderer))
        }
      case None => callback
    }
  }

  private def validatorImpl(value: Executor, expr: Iterable[Expr], context: Context): Observable[String] = {
    val renderer: Renderer = context.renderer
    value match {
      case _: IAtomic =>
        if (expr.isEmpty) {
          NONE
        } else {
          Observable(onError("expand", "Can't extract items over atomic element", renderer))
        }
      case IAsync(data) => data.flatMap(x => validatorImpl(x, expr, context))
      case IArray(data) => data.head.flatMap(v => validatorImpl(v, expr, context))
      case IOption(value) => value.fold(NONE)(v => validatorImpl(v, expr, context))
      case obj: IObject =>
        if (expr.isEmpty)

          Observable
            .fromIterable(obj.fields)
            .flatMap { case (name, value) =>
              if (value.isFunction) {
                Observable(onError("explicit", s"Function '$name' must be called explicitly.", renderer))
              } else {
                NONE
              }
            }
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
              val duple: String = exprFinal.map(_.id).groupBy(identity).collect { case (x, ys) if ys.size > 1 => s"'$x'" }.mkString(", ")
              Observable(onError("duplicated-key", s"Duplicated key '$duple'.", renderer))
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
                    Observable(onError("undefined", s"Fragment '$id' does not exists.", renderer))
                  }

                case FunctionExtractor(id, args, body) =>
                  obj.getField(id) match {
                    case None => Observable(onError("undefined", s"Function '$id' does not exists.", renderer))
                    case _ => NONE
                  }

                case ObjExtractor(id, body, directive) =>

                  validateDirective(directive, context) {
                    obj
                      .getField(id)
                      .fold(Observable(onError("undefined", s"Field '$id' does not exists.", renderer)))(value => validatorImpl(value, body, context))
                  }

                case Alias(id, body) =>

                  obj.getField(body.id)
                    .fold(Observable(onError("undefined", s"Function '$id' does not exists.", renderer))) { value =>
                      value
                        .call(extractArgs(body.args, context))
                        .fold(Observable(onError("unprocessable", s"Field '$id' is not a function.", renderer))) { result =>
                          result
                            .flatMap(v => validatorImpl(v, body.body, context))
                            .onErrorHandle(e => onError("function-call", e.getMessage, renderer))
                        }
                    }
              }
          }
        }
      case _ =>
        NONE
    }
  }

  private def eval(value: Executor, accessor: Accessor, expr: List[Expr], rend: Renderer): Observable[String] = {
    val (fragmentDefInit, others) = expr.partition(_.isInstanceOf[FragmentDef])
    val fragmentDef: Map[String, FragmentDef] = fragmentDefInit.collect { case x: FragmentDef => x }.map(x => x.id -> x).toMap
    val context: Context = Context(fragmentDef, accessor, rend)

    taskValidator(value, others, context).switchIfEmpty(projector(value, others, context))
    //projector(value, others, context)
  }

  private def introspection(binding: Binding): Try[Executor] = {

    def inputValueMapper(data: InputValue): Executor = {
      IObject(
        data.name,
        Seq(
          "type" -> typeMapper(data.`type`()),
          "name" -> IString(data.name),
          "description" -> IOption(data.description.map(IString)),
          "defaultValue" -> IOption(data.defaultValue.map(IString))
        )
      )
    }

    def enumMapper(enumValue: EnumValue): Executor = {
      IObject(
        enumValue.name,
        Seq(
          "name" -> IString(enumValue.name),
          "description" -> IOption(enumValue.description.map(IString)),
          "isDeprecated" -> IBoolean(enumValue.isDeprecated),
          "deprecationReason" -> IOption(enumValue.deprecationReason.map(IString))
        )
      )
    }

    def fieldMapper(field: Field): Executor = {
      IObject(
        field.name,
        Seq(
          "name" -> IString(field.name),
          "type" -> typeMapper(field.`type`()),
          "description" -> IOption(field.description.map(IString)),
          "args" -> IArray(Observable.fromIterable(field.args).map(inputValueMapper)),
          "isDeprecated" -> IBoolean(field.isDeprecated),
          "deprecationReason" -> IOption(field.deprecationReason.map(IString))
        )
      )
    }

    def typeMapper(t: Type): Executor = {
      IObject(
        t.name.getOrElse("undefined"),
        Seq(
          "kind" -> IString(t.kind.toString),
          "name" -> IOption(t.name.map(IString)),
          "description" -> IOption(t.description.map(IString)),
          "fields" -> new IFunction {
            override def checkArgs: Boolean = false

            override def argumentsLength: Int = 0

            override def invoke(args: Seq[String]): Observable[Executor] = Observable.pure {
              val includeDeprecated: Boolean = args.headOption.forall(_ == "true")
              if (includeDeprecated) {
                IArray(Observable.fromIterable(t.fields()).map(fieldMapper))
              } else {
                IArray(Observable.fromIterable(t.fields()).filterNot(_.isDeprecated).map(fieldMapper))
              }
            }
          },
          "interfaces" -> IArray(Observable.fromIterable(t.interfaces()).map(typeMapper)),
          "possibleTypes" -> IArray(Observable.fromIterable(t.possibleTypes()).map(typeMapper)),
          "enumValues" -> new IFunction {
            override def argumentsLength: Int = 1

            override def invoke(args: Seq[String]): Observable[Executor] = Observable.pure(IArray(Observable.fromIterable(t.enumValues).map(enumMapper)))
          },
          "inputFields" -> IArray(Observable.fromIterable(t.inputFields()).map(inputValueMapper)),
          "ofType" -> IOption(t.ofType().map(typeMapper))
        )
      )
    }

    def collectTypes(t: Type): Map[String, Type] = {

      val recursion: Map[String, Type] = Seq(
        t.fields().map(_.`type`()).foldLeft(MAP_EMPTY) { case (acc, x) => acc ++ collectTypes(x) },
        t.inputFields().map(_.`type`).foldLeft(MAP_EMPTY) { case (acc, x) => acc ++ collectTypes(x()) },
        t.interfaces().foldLeft(MAP_EMPTY) { case (acc, x) => acc ++ collectTypes(x) },
        t.ofType().fold(MAP_EMPTY)(collectTypes),
        t.possibleTypes().foldLeft(MAP_EMPTY) { case (acc, x) => acc ++ collectTypes(x) }
      ).reduce(_ ++ _)

      if ((!recursion.contains(t.name)) && (t.isObject || t.isTrait || t.isScalar)) {
        t.name.fold(recursion)(name => recursion + (name -> t))
      } else {
        recursion
      }
    }

    def resolver(querySystem: Type, mutationSystem: Option[Type], binding: Binding): Executor = {
      val startPoint: Map[String, Type] = Introspection.ALL.flatMap(x => x.name.map(y => y -> x)).toMap
      val data: Map[String, Type] = binding.schema.map(x => collectTypes(x._type)).foldLeft(startPoint)(_ ++ _)
      val cache: String => Observable[Executor] = {
        val cache: mutable.Map[String, Executor] = mutable.Map.empty[String, Executor]
        (key: String) => {
          data.get(key) match {
            case Some(value) => Observable(cache.getOrElseUpdate(key, typeMapper(value)))
            case None => Observable.raiseError(new RuntimeException(s"Undefined type '$key'"))
          }
        }
      }

      IObject(
        "data",
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
              "description" -> IOption(None), // fixme
              "types" -> IArray(Observable.fromIterable(data.keys).flatMap(cache) ++ Observable.fromIterable(Introspection.ALL).map(typeMapper)),
              "queryType" -> typeMapper(querySystem),
              "mutationType" -> IOption(mutationSystem.map(typeMapper)),
              "subscriptionType" -> IOption(None), // todo
              "directives" -> IArray(EXECUTOR_EMPTY) // todo
            )
          )
        )
      )
    }

    def joinTypes(aType: Type, bType: Type): Type = {
      if (aType.kind == bType.kind) {
        Type(
          aType.kind,
          Seq(aType.name, bType.name).mkString(", "),
          Some(Seq(aType.description, bType.description).flatten.mkString(", ")),
          () => aType.fields() ++ bType.fields(),
          () => aType.interfaces() ++ bType.interfaces(),
          () => aType.possibleTypes() ++ bType.possibleTypes(),
          aType.enumValues ++ bType.enumValues,
          () => aType.inputFields() ++ bType.inputFields(),
          () => None
        )
      } else {
        logger.warn(s"Types '${aType.name}' and ${bType.name} has no same kind.")
        aType
      }
    }

    def joinByScope(binding: Binding, scope: Scope): Option[Type] = {
      binding.getBy(scope).map(_._type).reduceOption[Type] { case (a, b) => joinTypes(a, b) }
    }

    val querySystem: Option[Type] = joinByScope(binding, QUERY_SCOPE)
    val mutationSystem: Option[Type] = joinByScope(binding, MUTATION_SCOPE)

    querySystem match {
      case Some(query) => Try(resolver(query, mutationSystem, binding))
      case None => Failure(new RuntimeException("Query jsoft.graphql.model must be instantiate"))
    }
  }

  def interpreter(bind: Binding, link: Link = LinkImpl, renderer: Renderer = JsonRenderer)(implicit ec: ExecutionContext): (Map[String, String], Option[String]) => Observable[String] = {

    introspection(bind) match {
      case Failure(exception) =>

        logger.error(exception.getLocalizedMessage, exception)

        (_: Map[String, String], _: Option[String]) => {
          Observable.raiseError(new RuntimeException("System unavailable."))
        }

      case Success(introspectionExecutor) =>

        val valueSchema: Executor = bind.executor.joinFields(introspectionExecutor)
        (queryParams: Map[String, String], body: Option[String]) => {

          Observable
            .fromTask(Task.fromFuture(link.build(queryParams, body)))
            .flatMap { accessor =>
              accessor.query match {
                case Some(value) =>
                  Observable
                    .fromTask(Parser.processor(value))
                    .flatMap { xs =>
                      eval(valueSchema, accessor, xs, renderer)
                    }
                case None => Observable.raiseError(new RuntimeException("query or mutation required"))
              }
            }
        }
    }

  }

}
