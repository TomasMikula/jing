package jing.openapi.model.client

import jing.openapi.model.*
import jing.openapi.model.RequestSchema.ConstantPath

import scala.NamedTuple.NamedTuple
import scala.annotation.implicitNotFound

class ClientEndpoint[Is, O](
  private val underlying: HttpEndpoint[Is, O],
) {
  import ClientEndpoint.*

  /** Used to supply parameter values to this endpoint.
   *
   * If this endpoint does not take parameters, does not compile.
   */
  def params[Ps, Rest](using
    @implicitNotFound(
      "Cannot prove that endpoint input starts with `Obj`-typed \"params\" field.\n" +
      "Expected:\n" +
      "  Void || \"params\" :: Obj[...] || ...\n" +
      "Actual:\n" +
      "  ${Is}."
    )
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
    /** Allows to supply body to this endpoint, turning it into a fully constructed [[HttpRequest]].
     *
     * Returns the auxiliary type [[PendingBody]],
     * because IDE hints for possible Mime Types work better that way.
     */
    def body: PendingBody[Bs, DiscriminatorOf[Bs], O] =
      PendingBody(endpoint)

  // Revisit the need for this intermediary after
  // https://github.com/scalameta/metals/issues/7556
  // is fixed. If persists, report a new ticket, specifically for match type reduction.
  class PendingBody[Bs, MT <: DiscriminatorOf[Bs], O](
    endpoint: ClientEndpoint[Void || "body" :: DiscriminatedUnion[Bs], O],
  ) {
    /** Takes a Mime Type (as type argument) and a body corresponding to that Mime Type.
     *
     * Returns a fully constructed [[HttpRequest]].
     */
    def apply[MimeType <: MT](using i: MimeType IsCaseOf Bs)(
      body: Value[i.Type],
    ): HttpRequest[MimeType, O] =
      import endpoint.underlying.{method, responseSchema}
      val (path, bodySchema) =
        endpoint.underlying.requestSchema match
          case RequestSchema.WithBody(RequestSchema.ConstantPath(path), schema) =>
            (path, schema)
      HttpRequest(
        method,
        paramsSchema = RequestSchema.Params.ConstantPath(path),
        params = Value.Obj.empty,
        body = Some((bodySchema, Body(i, body))),
        responseSchema,
      )
  }

  class PendingParams[Is, Ps, PNames <: PropNamesTuple[Ps], PTypes <: PropTypesTupleU[Value, Ps], Rest, O](
    endpoint: ClientEndpoint[Is, O],
  ) {
    /** Accepts request parameters as an `Obj`-typed value. */
    def fromValue(
      params: Value[Obj[Ps]],
    ): ClientEndpoint.WithQueryParams[Is, Ps, Rest, O] =
      WithQueryParams(endpoint, params)

    /** Accepts request parameters as a named tuple. */
    def apply(t: NamedTuple[PNames, PTypes])(using PropertyList[Ps]): ClientEndpoint.WithQueryParams[Is, Ps, Rest, O] =
      fromValue(Value.Obj(_(t.toTuple)))

    /** Allows to supply request parameters one-by-one via a builder provided to the function `f`. */
    def builder(
      f: Value.ObjectBuilder[Void, ToRightAssoc[Ps]] => Value.ObjectBuilder[Ps, Void],
    ): ClientEndpoint.WithQueryParams[Is, Ps, Rest, O] =
      fromValue(f(Value.ObjectBuilder[Ps]).result)
  }

  sealed trait RequestBuilder[Is, Acc, Remaining, O] {
    def toRequest(using
      ev1: Acc =:= Is,
      @implicitNotFound("Cannot prove that there are no more inputs to the endpoint, namely that ${Remaining} =:= Void.")
      ev2: Remaining =:= Void,
    ): HttpRequest[Nothing, O]
  }

  class WithQueryParams[Is, Ps, Remaining, O](
    endpoint: ClientEndpoint[Is, O],
    params: Value[Obj[Ps]],
  ) extends RequestBuilder[Is, Void || "params" :: Obj[Ps], Remaining, O] {
    // TODO: body method

    override def toRequest(using
      ev1: (Void || "params" :: Obj[Ps]) =:= Is,
      ev2: Remaining =:= Void,
    ): HttpRequest[Nothing, O] =
      import endpoint.underlying.{method, responseSchema}
      val paramsSchema: RequestSchema.Params[Ps] =
        ev1.substituteContra(endpoint.underlying.requestSchema) match
          case RequestSchema.Parameterized(params) => params
      HttpRequest(
        method,
        paramsSchema = paramsSchema,
        params = params,
        body = None,
        responseSchema,
      )
  }

  object WithQueryParams {
    extension [Ps, O](wqp: WithQueryParams[Void || "params" :: Obj[Ps], Ps, Void, O]) {
      /** Executes this request against the given API URL, using a `given Client`. */
      def runAgainst(apiBaseUrl: String)(using
        @implicitNotFound(
          "No given Client instance in scope to run this request.\n" +
          "To use the default Java HTTP client, import jing.openapi.client.default.instance " +
          "(you might need to add \"dev.continuously.jing\" %% \"jing-openapi-client-default\" % \"<version>\" to library dependencies).\n" +
          "Other Client instances might be available via integrations.\n"
        )
        client: Client,
      ): client.Response[O] =
        wqp.toRequest.runAgainst(apiBaseUrl)
    }
  }
}
