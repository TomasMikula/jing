package jing.openapi.model

import libretto.lambda.util.SingletonType

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
      case Primitive(value: SchemaMotif.Primitive[Nothing, T])
      case PrimitiveArray[T](elem: SchemaMotif.Primitive[Nothing, T]) extends QueryParamSchema[Arr[T]]
      case Unsupported[S <: String](msg: SingletonType[S]) extends QueryParamSchema[Oops[S]]

    object QueryParamSchema {
      def unsupported(msg: String): QueryParamSchema[Oops[msg.type]] =
        Unsupported(SingletonType(msg))
    }
  }

  sealed trait Path[Ps]

  object Path {
    case class Constant(value: String) extends Path[Void]

    sealed trait Parameterized[Ps] extends Path[Ps]

    case class WithParam[Init, ParamName <: String, ParamType](
      prefix: Path[Init],
      pName: SingletonType[ParamName],
      pSchema: ParamSchema[ParamType],
      suffix: String,
    ) extends Parameterized[Init || ParamName :: ParamType]

    sealed trait ParamSchema[T] {
      import ParamSchema.*

      def toQueryParamSchema: Params.QueryParamSchema[T] =
        this match
          case Primitive(p) => Params.QueryParamSchema.Primitive(p)
          case Unsupported(msg) => Params.QueryParamSchema.Unsupported(msg)
    }
    object ParamSchema {
      case class Primitive[T](value: SchemaMotif.Primitive[Nothing, T]) extends ParamSchema[T]
      case class Unsupported[S <: String](msg: SingletonType[S]) extends ParamSchema[Oops[S]]

      def unsupported(msg: String): ParamSchema[Oops[msg.type]] =
        Unsupported(SingletonType(msg))
    }
  }
}
