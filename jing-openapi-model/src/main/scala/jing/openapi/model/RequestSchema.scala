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
      pSchema: Schema[ParamType],
    ) extends Params.Proper[Ps || ParamName :: ParamType]

    case class WithQueryParamOpt[Ps, ParamName <: String, ParamType](
      init: Params[Ps],
      pName: SingletonType[ParamName],
      pSchema: Schema[ParamType],
    ) extends Params.Proper[Ps || ParamName :? ParamType]
  }

  sealed trait Path[Ps]

  object Path {
    case class Constant(value: String) extends Path[Void]

    sealed trait Parameterized[Ps] extends Path[Ps]

    case class WithParam[Init, ParamName <: String, ParamType](
      prefix: Path[Init],
      pName: SingletonType[ParamName],
      pSchema: Schema[ParamType],
      suffix: String,
    ) extends Parameterized[Init || ParamName :: ParamType]
  }
}
