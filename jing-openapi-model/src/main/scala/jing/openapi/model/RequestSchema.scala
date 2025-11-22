package jing.openapi.model

import libretto.lambda.util.SingletonType
import scala.NamedTuple.NamedTuple

/** Schema of request data, i.e. request parameters and request body.
 *
 * @tparam Is named list of request inputs, separated by [[||]]
 */
sealed trait RequestSchema[Is]

object RequestSchema {
  sealed trait ParamsOpt[Is] extends RequestSchema[Is]

  case class WithBody[Ps, B](
    params: ParamsOpt[Ps],
    body: BodySchema.NonEmpty[B],
  ) extends RequestSchema[Ps || "body" :: B]

  case class ConstantPath(path: String) extends ParamsOpt[Void]

  case class Parameterized[Ps](
    params: Params.Proper[Ps],
  ) extends ParamsOpt[Void || "params" :: Obj[Ps]]

  sealed trait Params[Ps]

  object Params {
    case class ConstantPath(path: String) extends Params[Void]

    sealed trait Proper[Ps] extends Params[Ps]

    case class ParameterizedPath[Ps](
      path: Path.Parameterized[Ps],
    ) extends Proper[Ps]

    case class WithQueryParam[Ps, ParamName <: String, ParamType](
      init: Params[Ps],
      pName: SingletonType[ParamName],
      pSchema: QueryParamSchema[ParamType],
    ) extends Params.Proper[Ps || ParamName :: ParamType]

    case class WithQueryParamOpt[Ps, ParamName <: String, ParamType](
      init: Params[Ps],
      pName: SingletonType[ParamName],
      pSchema: QueryParamSchema[ParamType],
    ) extends Params.Proper[Ps || ParamName :? ParamType]

    enum QueryParamSchema[T]:
      case Primitive(value: SchemaMotif.Primitive[Nothing, T], format: QueryParamSchema.Format)
      case PrimitiveArray[T](elem: SchemaMotif.Primitive[Nothing, T], format: QueryParamSchema.Format) extends QueryParamSchema[Arr[T]]
      case Unsupported[S <: String](msg: SingletonType[S]) extends QueryParamSchema[Oops[S]]

    object QueryParamSchema {
      def unsupported(msg: String): QueryParamSchema[Oops[msg.type]] =
        Unsupported(SingletonType(msg))
      case class Format(style: Style, explode: Explode)

      object Format {
        def apply(style: Option[String], explode: Option[Boolean]): Either[Unsupported[?], Format] =
          for {
            style   <- Style(style)
            explode <- Explode(explode)
          } yield
            Format(style, explode)
      }

      enum Style:
        case Form

      object Style {
        def default: Style =
          Form

        def apply(style: Option[String]): Either[Unsupported[?], Style] =
          style match
            case None    => Right(default)
            case Some(s) => fromString(s) match
              case Some(s) => Right(s)
              case None    => Left(Unsupported(SingletonType(s"Style $s is not (yet?) supported for query parameters.")))

        def fromString(s: String): Option[Style] =
          s match
            case "form" => Some(Form)
            case _ => None
      }

      enum Explode:
        case True

      object Explode {
        def default: Explode =
          True

        def apply(explode: Option[Boolean]): Either[Unsupported[?], Explode] =
          explode match
            case None    => Right(default)
            case Some(b) => b match
              case true      => Right(True)
              case e @ false => Left(Unsupported(SingletonType(s"explode = $e is not yet supported for query parameters.")))
      }
    }
  }

  sealed trait Path[Ps] {
    def print(args: Value[Obj[Ps]]): String

    def instantiate: Path.PendingParams[Ps, PropNamesTuple[Ps], PropTypesTupleU[Value, Ps]] =
      new Path.PendingParams(this)
  }

  object Path {
    case class Constant(value: String) extends Path[Void] {
      override def print(args: Value[Obj[Void]]): String = value
    }

    sealed trait Parameterized[Ps] extends Path[Ps]

    case class WithParam[Init, ParamName <: String, ParamType](
      prefix: Path[Init],
      pName: SingletonType[ParamName],
      pSchema: ParamSchema[ParamType],
      suffix: String,
    ) extends Parameterized[Init || ParamName :: ParamType] {
      override def print(args: Value[Obj[Init || ParamName :: ParamType]]): String =
        import Value.*
        val (initArgs, lastArg) = args.unsnoc
        prefix.print(initArgs) + pSchema.printArg(lastArg) + suffix
    }

    class PendingParams[Ps, PNames <: PropNamesTuple[Ps], PTypes <: PropTypesTupleU[Value, Ps]](
      path: Path[Ps],
    ) {
      def fromValue(params: Value[Obj[Ps]]): String =
        path.print(params)

      def apply(t: NamedTuple[PNames, PTypes])(using PropertyList[Ps]): String =
        fromValue(Value.Obj(_(t.toTuple)))
    }

    sealed trait ParamSchema[T] {
      def printArg(arg: Value[T]): String
    }

    object ParamSchema {
      case class Primitive[T](
        value: SchemaMotif.Primitive[Nothing, T],
        format: Format,
      ) extends ParamSchema[T] {
        override def printArg(arg: Value[T]): String =
          printPrimitive(value, arg)
      }

      case class Unsupported[S <: String](msg: SingletonType[S]) extends ParamSchema[Oops[S]] {
        override def printArg(arg: Value[Oops[S]]): String =
          arg.isNotOops
      }

      def unsupported(msg: String): ParamSchema[Oops[msg.type]] =
        Unsupported(SingletonType(msg))

      private def printPrimitive[T](
        schema: SchemaMotif.Primitive[Nothing, T],
        arg: Value[T],
      ): String =
        schema match
          case SchemaMotif.I32() => (arg: Value[Int32]).intValue.toString
          case SchemaMotif.I64() => (arg: Value[Int64]).longValue.toString
          case SchemaMotif.S() => (arg: Value[Str]).stringValue
          case SchemaMotif.B() => (arg: Value[Bool]).booleanValue.toString
          case e: SchemaMotif.Enumeration[Nothing, t, cases] =>
            printPrimitive[t](e.baseType, (arg: Value[Enum[t, cases]]).widenEnum)

      case class Format(style: Style, explode: Explode)
      object Format {
        def apply(style: Option[String], explode: Option[Boolean]): Either[Unsupported[?], Format] =
          for {
            style   <- Style(style)
            explode <- Explode(explode)
          } yield
            Format(style, explode)
      }

      enum Style:
        case Simple

      object Style {
        def default: Style =
          Simple

        def apply(style: Option[String]): Either[Unsupported[?], Style] =
          style match
            case None    => Right(default)
            case Some(s) => fromString(s) match
              case Some(s) => Right(s)
              case None    => Left(Unsupported(SingletonType(s"Style '$s' is not (yet?) supported for path parameters.")))

        def fromString(s: String): Option[Style] =
          s match
            case "simple" => Some(Simple)
            case _ => None
      }

      enum Explode:
        case False

      object Explode {
        def default: Explode =
          False

        def apply(explode: Option[Boolean]): Either[Unsupported[?], Explode] =
          explode match
            case None    => Right(default)
            case Some(b) => b match
              case false    => Right(False)
              case e @ true => Left(Unsupported(SingletonType(s"explode = $e is not yet supported for path parameters.")))
      }
    }
  }
}
