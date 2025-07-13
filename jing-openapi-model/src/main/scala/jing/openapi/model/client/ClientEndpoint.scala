package jing.openapi.model.client

import jing.openapi.model.*
import jing.openapi.model.RequestSchema.ConstantPath

import scala.NamedTuple.NamedTuple

class ClientEndpoint[Is, O](
  private val underlying: HttpEndpoint[Is, O],
) {
  import ClientEndpoint.*

  def params[Ps, Rest](using
    ev: ToRightAssoc[Is] =:= ("params" :: Obj[Ps] || Rest),
  ): PendingParams[Is, Ps, PropNamesTuple[Ps], PropTypesTupleU[Value, Ps], Rest, O] =
    PendingParams[Is, Ps, PropNamesTuple[Ps], PropTypesTupleU[Value, Ps], Rest, O](this)
}

object ClientEndpoint {
  given endpointInterpreter: (HttpEndpoint.Interpreter { type Endpoint[A, B] = ClientEndpoint[A, B] }) =
    new HttpEndpoint.Interpreter {
      override type Endpoint[I, O] = ClientEndpoint[I, O]

      def interpret[I, O](ep: HttpEndpoint[I, O]): Endpoint[I, O] =
        ClientEndpoint(ep)
    }

  extension [Bs, O](endpoint: ClientEndpoint[Void || "body" :: DiscriminatedUnion[Bs], O])
    def body[MimeType <: String](using i: MimeType IsCaseOf Bs)(
      body: Value[i.Type],
    ): HttpThunk[MimeType, O] =
      import endpoint.underlying.{method, responseSchema}
      val (path, bodySchema) =
        endpoint.underlying.requestSchema match
          case RequestSchema.WithBody(RequestSchema.ConstantPath(path), schema) =>
            (path, schema)
      HttpThunk(
        method,
        paramsSchema = RequestSchema.Params.ConstantPath(path),
        params = Value.obj.empty,
        body = Some((bodySchema, Body(i, body))),
        responseSchema,
      )

  class PendingParams[Is, Ps, PNames <: PropNamesTuple[Ps], PTypes <: PropTypesTupleU[Value, Ps], Rest, O](
    endpoint: ClientEndpoint[Is, O],
  ) {
    def fromValue(
      params: Value[Obj[Ps]],
    ): ClientEndpoint.WithQueryParams[Is, Ps, Rest, O] =
      WithQueryParams(endpoint, params)

    def fromNamedTuple(t: NamedTuple[PNames, PTypes])(using PropertyList[Ps]): ClientEndpoint.WithQueryParams[Is, Ps, Rest, O] =
      fromValue(Value.obj(_(t.toTuple)))

    def apply(
      f: Value.ObjectBuilder[Void, ToRightAssoc[Ps]] => Value.ObjectBuilder[Ps, Void],
    ): ClientEndpoint.WithQueryParams[Is, Ps, Rest, O] =
      fromValue(f(Value.ObjectBuilder[Ps]).result)
  }

  sealed trait RequestBuilder[Is, Acc, Remaining, O] {
    def toRequest(using ev1: Acc =:= Is, ev2: Remaining =:= Void): HttpThunk[Nothing, O]
  }

  class WithQueryParams[Is, Qs, Remaining, O](
    endpoint: ClientEndpoint[Is, O],
    params: Value[Obj[Qs]],
  ) extends RequestBuilder[Is, Void || "params" :: Obj[Qs], Remaining, O] {
    // TODO: body method

    override def toRequest(using
      ev1: (Void || "params" :: Obj[Qs]) =:= Is,
      ev2: Remaining =:= Void,
    ): HttpThunk[Nothing, O] =
      import endpoint.underlying.{method, responseSchema}
      val paramsSchema: RequestSchema.Params[Qs] =
        ev1.substituteContra(endpoint.underlying.requestSchema) match
          case RequestSchema.Parameterized(params) => params
      HttpThunk(
        method,
        paramsSchema = paramsSchema,
        params = params,
        body = None,
        responseSchema,
      )

    def runAgainst(using (Void || "params" :: Obj[Qs]) =:= Is, Remaining =:= Void)(
      apiBaseUrl: String,
    )(using
      client: Client,
    ): client.Response[O] = {
      toRequest.runAgainst(apiBaseUrl)
    }
  }
}
