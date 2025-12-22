package jing.openapi

import jing.openapi.model.*
import jing.openapi.model.IsPropertyOf.IsRequiredPropertyOf
import libretto.lambda.{Items1, Items1Named}
import libretto.lambda.util.{Applicative, Exists, SingletonType, TypeEq, TypeEqK}
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq.Refl
import scala.quoted.*

object ModelToScalaAst {
  given ToExpr[HttpMethod] with {
    override def apply(x: HttpMethod)(using Quotes): Expr[HttpMethod] =
      x match
        case HttpMethod.Get     => '{ HttpMethod.Get }
        case HttpMethod.Post    => '{ HttpMethod.Post }
        case HttpMethod.Put     => '{ HttpMethod.Put }
        case HttpMethod.Delete  => '{ HttpMethod.Delete }
        case HttpMethod.Head    => '{ HttpMethod.Head }
        case HttpMethod.Options => '{ HttpMethod.Options }
        case HttpMethod.Patch   => '{ HttpMethod.Patch }
        case HttpMethod.Trace   => '{ HttpMethod.Trace }

  }

  def quotedHttpEndpoint[Is, O](
    x: HttpEndpoint[Is, O],
  )(using
    Quotes,
  ): (Type[HttpEndpoint[Is, O]], Expr[HttpEndpoint[Is, O]]) =
    val HttpEndpoint(meth, req, resp) = x
    val (reqType, reqExpr) = quotedRequestSchema(req)
    val (respType, respExpr) = quotedResponseSchema(resp)

    given Type[Is] = reqType
    given Type[O] = respType

    (
      Type.of[HttpEndpoint[Is, O]],
      '{ HttpEndpoint(${Expr(meth)}, ${reqExpr}, ${respExpr}) },
    )

  def quotedRequestSchema[T](
    x: RequestSchema[T],
  )(using
    Quotes,
  ): (Type[T], Expr[RequestSchema[T]]) =
    x match
      case ps: RequestSchema.ParamsOpt[ps] =>
        summon[T =:= ps]
        quotedRequestSchemaParamsOpt(ps)
      case pb: RequestSchema.WithBody[ps, b] =>
        summon[T =:= (ps || "body" :: b)]
        val (pst, ps) = quotedRequestSchemaParamsOpt(pb.params)
        val (bt, b) = quotedBodySchemaNonEmpty(pb.body)
        given Type[ps] = pst
        given Type[b] = bt
        (
          Type.of[ps || "body" :: b],
          '{ RequestSchema.WithBody($ps, $b) },
        )

  def quotedRequestSchemaParamsOpt[Ps](
    x: RequestSchema.ParamsOpt[Ps],
  )(using
    Quotes,
  ): (Type[Ps], Expr[RequestSchema.ParamsOpt[Ps]]) =
    x match
      case RequestSchema.ConstantPath(path) =>
        summon[Ps =:= Void]
        (Type.of[Void], '{ RequestSchema.ConstantPath(${Expr(path)}) })
      case p: RequestSchema.Parameterized[ps] =>
        summon[Ps =:= (Void || "params" :: Obj[ps])]
        val (t, ps) = quotedParamsProper(p.params)
        given Type[ps] = t
        (Type.of[Void || "params" :: Obj[ps]], '{ RequestSchema.Parameterized($ps) })

  def quotedParams[Ps](
    ps: RequestSchema.Params[Ps],
  )(using
    Quotes,
  ): (Type[Ps], Expr[RequestSchema.Params[Ps]]) =
    ps match
      case RequestSchema.Params.ConstantPath(path) =>
        (Type.of[Void], '{ RequestSchema.Params.ConstantPath(${Expr(path)}) })
      case proper: RequestSchema.Params.Proper[Ps] =>
        quotedParamsProper(proper)

  def quotedParamsProper[Ps](
    ps: RequestSchema.Params.Proper[Ps],
  )(using
    Quotes,
  ): (Type[Ps], Expr[RequestSchema.Params.Proper[Ps]]) =
    ps match
      case wqp: RequestSchema.Params.WithQueryParam[init, pname, ptype] =>
        val (initt, inits) = quotedParams(wqp.init)
        val (pnameTp, pnameEx) = quotedSingletonString(wqp.pName)
        val (pt, ps) = quotedQueryParamSchema(wqp.pSchema)
        given Type[init] = initt
        given Type[pname] = pnameTp
        given Type[ptype] = pt
        (
          Type.of[init || pname :: ptype],
          '{ RequestSchema.Params.WithQueryParam($inits, ${Expr(wqp.pName)}, ${ps}) },
        )
      case wqpo: RequestSchema.Params.WithQueryParamOpt[init, pname, ptype] =>
        val (initt, inits) = quotedParams(wqpo.init)
        val (pnameTp, pnameEx) = quotedSingletonString(wqpo.pName)
        val (pt, ps) = quotedQueryParamSchema(wqpo.pSchema)
        given Type[init] = initt
        given Type[pname] = pnameTp
        given Type[ptype] = pt
        (
          Type.of[init || pname :? ptype],
          '{ RequestSchema.Params.WithQueryParamOpt($inits, ${Expr(wqpo.pName)}, ${ps}) },
        )
      case pp: RequestSchema.Params.ParameterizedPath[ps] =>
        ???

  def quotedQueryParamSchema[T](
    schema: RequestSchema.Params.QueryParamSchema[T],
  )(using
    Quotes,
  ): (Type[T], Expr[RequestSchema.Params.QueryParamSchema[T]]) =
    schema match
      case RequestSchema.Params.QueryParamSchema.Primitive(s, fmt) =>
        val (t, e) = quotedSchemaMotifPrimitive(s)
        given Type[T] = t
        (t, '{ RequestSchema.Params.QueryParamSchema.Primitive($e, ${quotedQueryParamFormat(fmt)}) })
      case a: RequestSchema.Params.QueryParamSchema.PrimitiveArray[t] =>
        val (t, e) = quotedSchemaMotifPrimitive(a.elem)
        given Type[t] = t
        (Type.of[Arr[t]], '{ RequestSchema.Params.QueryParamSchema.PrimitiveArray($e, ${quotedQueryParamFormat(a.format)}) })
      case u: RequestSchema.Params.QueryParamSchema.Unsupported[msg] =>
        val (t, e) = quotedSingletonString(u.msg)
        given Type[msg] = t
        (Type.of[Oops[msg]], '{ RequestSchema.Params.QueryParamSchema.Unsupported($e) })

  def quotedPathParamSchema[T](
    schema: RequestSchema.Path.ParamSchema[T],
  )(using
    Quotes,
  ): (Type[T], Expr[RequestSchema.Path.ParamSchema[T]]) =
    schema match
      case RequestSchema.Path.ParamSchema.Primitive(s, fmt) =>
        val (t, e) = quotedSchemaMotifPrimitive(s)
        given Type[T] = t
        (t, '{ RequestSchema.Path.ParamSchema.Primitive($e, ${quotedPathParamFormat(fmt)}) })
      case u: RequestSchema.Path.ParamSchema.Unsupported[msg] =>
        val (t, e) = quotedSingletonString(u.msg)
        given Type[msg] = t
        (Type.of[Oops[msg]], '{ RequestSchema.Path.ParamSchema.Unsupported($e) })

  def quotedQueryParamFormat(fmt: RequestSchema.Params.QueryParamSchema.Format)(using Quotes): Expr[RequestSchema.Params.QueryParamSchema.Format] =
    fmt match
      case RequestSchema.Params.QueryParamSchema.Format(style, explode) =>
        '{ RequestSchema.Params.QueryParamSchema.Format(${quotedQueryParamStyle(style)}, ${quotedQueryParamExplode(explode)}) }

  def quotedQueryParamStyle(style: RequestSchema.Params.QueryParamSchema.Style)(using Quotes): Expr[RequestSchema.Params.QueryParamSchema.Style] =
    style match
      case RequestSchema.Params.QueryParamSchema.Style.Form => '{ RequestSchema.Params.QueryParamSchema.Style.Form }

  def quotedQueryParamExplode(explode: RequestSchema.Params.QueryParamSchema.Explode)(using Quotes): Expr[RequestSchema.Params.QueryParamSchema.Explode] =
    explode match
      case RequestSchema.Params.QueryParamSchema.Explode.True => '{ RequestSchema.Params.QueryParamSchema.Explode.True }

  def quotedPathParamFormat(fmt: RequestSchema.Path.ParamSchema.Format)(using Quotes): Expr[RequestSchema.Path.ParamSchema.Format] =
    fmt match
      case RequestSchema.Path.ParamSchema.Format(style, explode) =>
        '{ RequestSchema.Path.ParamSchema.Format(${quotedPathParamStyle(style)}, ${quotedPathParamExplode(explode)}) }

  def quotedPathParamStyle(style: RequestSchema.Path.ParamSchema.Style)(using Quotes): Expr[RequestSchema.Path.ParamSchema.Style] =
    style match
      case RequestSchema.Path.ParamSchema.Style.Simple => '{ RequestSchema.Path.ParamSchema.Style.Simple }

  def quotedPathParamExplode(explode: RequestSchema.Path.ParamSchema.Explode)(using Quotes): Expr[RequestSchema.Path.ParamSchema.Explode] =
    explode match
      case RequestSchema.Path.ParamSchema.Explode.False => '{ RequestSchema.Path.ParamSchema.Explode.False }

  def quotedResponseSchema[T](
    x: ResponseSchema[T],
  )(using
    Quotes,
  ): (Type[T], Expr[ResponseSchema[T]]) =
    x match
      case rs: ResponseSchema[as] =>
        val (t, e) =
          quotedNamedProduct(
            rs.schemasByStatusCode,
            [A] => bs => quotedBodySchema(bs),
          )
        given Type[as] = t
        (Type.of[as], '{ ResponseSchema($e) })

  def quotedBodySchema[T](
    s: BodySchema[T],
  )(using
    Quotes,
  ): (Type[T], Expr[BodySchema[T]]) =
    s match
      case BodySchema.Empty =>
        (Type.of[Unit], '{ BodySchema.Empty })
      case ne: BodySchema.NonEmpty[t] =>
        quotedBodySchemaNonEmpty(ne)

  def quotedBodySchemaNonEmpty[T](
    s: BodySchema.NonEmpty[T],
  )(using
    Quotes,
  ): (Type[T], Expr[BodySchema.NonEmpty[T]]) =
    s match
      case v: BodySchema.Variants[cases] =>
        val (t, e) =
          quotedNamedProduct(
            v.byMediaType,
            [A] => s => quotedSchema(s),
          )
        given Type[cases] = t
        (
          Type.of[DiscriminatedUnion[cases]],
          '{ BodySchema.Variants($e) },
        )

  def quotedSchema[T](s: Schema[T])(using Quotes): (Type[T], Expr[Schema[T]]) =
    quotedSchema_A[T, [x] =>> x](s)

  private def quotedSchema_A[T, N[_]](s: Schema[T])(using
    q: Quotes,
    N: Applicative[N],
  ): (Type[T], N[Expr[Schema[T]]]) =
    s match
      case Schema.Proper(s) =>
        val (tpe, nExp) = quotedSchemaMotif_A(s, [A] => sa => quotedSchema_A[A, N](sa))
        given Type[T] = tpe
        (tpe, nExp.map { exp => '{ Schema.Proper($exp) } })
      case u: Schema.Unsupported[msg] =>
        val (t, e) = quotedSchemaOops(u.message)
        (t, N.pure(e))

  private def quotedSchemaOops[S <: String](s: SingletonType[S])(using Quotes): (Type[Oops[S]], Expr[Schema[Oops[S]]]) =
    val (tpe, exp) = quotedSingletonString(s)
    given Type[S] = tpe
    (Type.of[Oops[S]], '{ Schema.unsupported($exp) })

  given AutoTyping[Schema] {
    override def typeOf[A](sa: Schema[A])(using Quotes): Type[A] =
      quotedSchema_A[A, ConstUnit](sa)._1
  }

  given [L] => AutoTyping[Schema.Labeled[L, _]] {
    override def typeOf[A](sa: Schema.Labeled[L, A])(using Quotes): Type[A] =
      summon[AutoTyping[Schema]].typeOf(sa.stripLabels)
  }

  private type ConstUnit[A] = Unit

  private given Applicative[ConstUnit] {
    override def pure[A](a: A): Unit = ()

    extension [A](fa: Unit)
      override def map[B](f: A => B): Unit = ()
      override def zip[B](fb: Unit): Unit = ()
  }

  /** Quotes a [[Labeled.Schema]] as `Expr[Schema[?]]`, substituting each labeled schema by a lookup in [[SchemaLookup]].
   *
   * Since the looked up type may potentially be different from the one next to the label, the result type is existential.
   */
  def quotedSchemaWithReferences[Rel[_, _], F[_]](
    schema: Schema.Labeled[String, ?],
    schemaLookup: SchemaLookup[Rel, F],
  )(using
    Quotes,
    Compatible[Rel],
    ExprIso[Rel],
    Applicative[F],
  ): Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])] =
    quotedSchemaWithReferencesRel(schema, schemaLookup)
      match
        case Indeed((_, (t, e))) => Indeed((t, e))

  def quotedSchemaWithReferencesRel[A, Rel[_, _], F[_]](
    schema: Schema.Labeled[String, A],
    schemaLookup: SchemaLookup[Rel, F],
  )(using
    q: Quotes,
    Rel: Compatible[Rel],
    Iso: ExprIso[Rel],
    F: Applicative[F],
  ): Exists[[B] =>> (Rel[A, B], (Type[B], F[Expr[Schema[B]]]))] = {
    schema match
      case Schema.Labeled.Unlabeled(value) =>
        quotedSchemaMotifRelAA(
          value,
          [A] => ps => Reader(quotedSchemaWithReferencesRel[A, Rel, F](ps, _)),
        ).run(schemaLookup) match {
          case ex @ Indeed(rel, (tpe, expr)) =>
            given Type[ex.T] = tpe
            Indeed((rel, (tpe, expr.map { expr => '{ Schema.Proper($expr) } })))
          }

      case Schema.Labeled.WithLabel(schemaName, schema) =>
        schemaLookup.lookupEntry(schemaName, schema) match
          case Indeed((rel, t, e)) =>
            Indeed((rel, (t, e)))

      case s: Schema.Labeled.Unsupported[String, msg] =>
        val (tpe, trm) = quotedSchemaOops(s.message)
        val rel: Oops[msg] `Rel` Oops[msg] =
          Rel.lift_singletonString(s.message).lift_oops
        Exists((rel, (tpe, F.pure(trm))))
  }

  def quotedSchemaObjectWithReferences[Rel[_, _], F[_]](
    schema: SchemaMotif.Object[Schema.Labeled[String, _], ?],
    schemaLookup: SchemaLookup[Rel, F],
  )(using
    q: Quotes,
    Rel: Compatible[Rel],
    Iso: ExprIso[Rel],
    F: Applicative[F],
  ): Exists[[Ps] =>> (Type[Ps], F[Expr[Schema[Obj[Ps]]]])] =
    quotedSchemaMotifObjectRelAA(
      schema.asObject.value,
      [A] => ps => Reader(quotedSchemaWithReferencesRel[A, Rel, F](ps, _))
    ).run(schemaLookup) match {
      case ex @ Indeed(_, (tpe, expr)) =>
        given Type[ex.T] = tpe
        Indeed((tpe, expr.map { expr => '{ Schema.Proper(SchemaMotif.Object($expr)) } }))
    }

  def quotedScalaValueOf[T, U](v: ScalaValueOf[T, U])(using Quotes): (Type[T], Expr[ScalaValueOf[T, U]]) =
    quotedScalaValueOfRefl(v, summon[Compatible[=:=]])
      ._2

  def quotedScalaValueOfRefl[T, U, Rel[_, _]](
    v: ScalaValueOf[T, U],
    Rel: Compatible[Rel],
  )(using
    Quotes,
  ): (Rel[T, T], (Type[T], Expr[ScalaValueOf[T, U]])) = {
    import ScalaValueOf.*

    v match
      case I32(i) =>
        summon[T <:< Int]
        val (tp, exp) = quotedSingletonInt(i)
        given Type[T] = tp
        (Rel.lift_singletonInt(i), (tp, '{ I32($exp) }))
      case I64(l) =>
        summon[T <:< Long]
        val (tp, exp) = quotedSingletonLong(l)
        given Type[T] = tp
        (Rel.lift_singletonLong(l), (tp, '{ I64($exp) }))
      case S(s) =>
        summon[T <:< String]
        val (tp, exp) = quotedSingletonString(s)
        given Type[T] = tp
        (Rel.lift_singletonString(s), (tp, '{ S($exp) }))
      case B(b) =>
        summon[T <:< Boolean]
        val (tp, exp) = quotedSingletonBoolean(b)
        given Type[T] = tp
        (Rel.lift_singletonBoolean(b), (tp, '{ B($exp) }))
  }

  def quotedSchemaMotifBasicPrimitive[F[_], T, G[_]](
    s: SchemaMotif.BasicPrimitive[F, T],
  )(using
    Quotes,
    Type[G],
  ): (Type[T], Expr[SchemaMotif.BasicPrimitive[G, T]]) =
    quotedSchemaMotifBasicPrimitiveRefl(s, summon[Compatible[=:=]])._2

  def quotedSchemaMotifBasicPrimitiveRefl[F[_], T, Rel[_, _], G[_]](
    s: SchemaMotif.BasicPrimitive[F, T],
    Rel: Compatible[Rel],
  )(using
    Quotes,
    Type[G],
  ): (Rel[T, T], (Type[T], Expr[SchemaMotif.BasicPrimitive[G, T]])) = {
    import jing.openapi.model.SchemaMotif.*

    s match
      case I32() => (Rel.lift_int32, (Type.of[Int32], '{ I32() }))
      case I64() => (Rel.lift_int64, (Type.of[Int64], '{ I64() }))
      case S()   => (Rel.lift_str, (Type.of[Str], '{ S() }))
      case B()   => (Rel.lift_bool, (Type.of[Bool], '{ B() }))
  }

  def quotedSchemaMotifPrimitive[F[_], T, G[_]](
    s: SchemaMotif.Primitive[F, T],
  )(using
    Quotes,
    Type[G],
  ): (Type[T], Expr[SchemaMotif.Primitive[G, T]]) =
    quotedSchemaMotifPrimitiveRefl(s, summon[Compatible[=:=]])
      ._2

  def quotedSchemaMotifPrimitiveRefl[F[_], T, Rel[_, _], G[_]](
    s: SchemaMotif.Primitive[F, T],
    Rel: Compatible[Rel],
  )(using
    Quotes,
    Type[G],
  ): (Rel[T, T], (Type[T], Expr[SchemaMotif.Primitive[G, T]])) = {
    import jing.openapi.model.SchemaMotif.{BasicPrimitive, Enumeration}

    s match
      case b: BasicPrimitive[F, T] =>
        quotedSchemaMotifBasicPrimitiveRefl(b, Rel)
      case e: Enumeration[F, base, cases] =>
        quotedSchemaMotifBasicPrimitiveRefl(e.baseType, Rel) match
          case (rb, (tb, b)) =>
            given Type[base] = tb
            quotedProductRefl(
              e.cases,
              [A] => (v: ScalaValueOf[A, base]) => quotedScalaValueOfRefl(v, Rel),
              Rel,
            ) match
              case (rCases, (tc, cs)) =>
                val rel: Enum[base, cases] `Rel` Enum[base, cases] =
                  Rel.lift_enum(rb, rCases)
                given Type[cases] = tc
                (rel, (Type.of[Enum[base, cases]], '{ Enumeration[G, base, cases](${b}, ${cs})}))
  }

  def quotedSchemaMotifConstant[F[_], T, G[_]](
    s: SchemaMotif.Constant[F, T]
  )(using
    Quotes,
    Type[G],
  ): (Type[T], Expr[SchemaMotif.Constant[G, T]]) =
    quotedSchemaMotifConstantRefl(s, summon[Compatible[=:=]])
      ._2

  def quotedSchemaMotifConstantRefl[F[_], T, Rel[_, _], G[_]](
    s: SchemaMotif.Constant[F, T],
    Rel: Compatible[Rel],
  )(using
    Quotes,
    Type[G],
  ): (Rel[T, T], (Type[T], Expr[SchemaMotif.Constant[G, T]])) =
    s match
      case SchemaMotif.Constant.Primitive(value) =>
        quotedScalaValueOfRefl(value, Rel) match
          case (r, (t, e)) =>
            given Type[T] = t
            (r, (t, '{ SchemaMotif.Constant.Primitive(${e: Expr[T ScalaValueOf ?]}) }))

  def quotedSchemaMotif[F[_], T](
    s: SchemaMotif[F, T],
    f: [A] => F[A] => (Type[A], Expr[F[A]]),
  )(using
    Quotes,
    AutoTyping[F],
    Faithful[F],
    Type[F],
  ): (Type[T], Expr[SchemaMotif[F, T]]) =
    quotedSchemaMotifRel[F, T, F, =:=](s, [A] => fa => Exists(summon[A =:= A], f(fa))) match
      case Indeed((ev, res)) =>
        ev.substituteContra[[X] =>> (Type[X], Expr[SchemaMotif[F, X]])](res)

  def quotedSchemaMotifRel[F[_], T, G[_], Rel[_, _]](
    s: SchemaMotif[F, T],
    f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))],
  )(using
    Quotes,
    AutoTyping[F],
    Faithful[F],
    Type[G],
    Compatible[Rel],
    ExprIso[Rel],
  ): Exists[[U] =>> (Rel[T, U], (Type[U], Expr[SchemaMotif[G, U]]))] =
    quotedSchemaMotifRelAA[F, T, G, Rel, [x] =>> x, [x] =>> x](s, f)

  def quotedSchemaMotif_A[F[_], T, G[_], N[_]](
    s: SchemaMotif[F, T],
    f: [A] => F[A] => (Type[A], N[Expr[G[A]]]),
  )(using
    Quotes,
    AutoTyping[F],
    Faithful[F],
    Type[G],
    Applicative[N],
  ): (Type[T], N[Expr[SchemaMotif[G, T]]]) =
    quotedSchemaMotifAA[F, T, G, [x] =>> x, N](s, f)

  def quotedSchemaMotifAA[F[_], T, G[_], M[_], N[_]](
    s: SchemaMotif[F, T],
    f: [A] => F[A] => M[(Type[A], N[Expr[G[A]]])],
  )(using
    Quotes,
    AutoTyping[F],
    Faithful[F],
    Type[G],
    Applicative[M],
    Applicative[N],
  ): M[(Type[T], N[Expr[SchemaMotif[G, T]]])] =
    quotedSchemaMotifRelAA[F, T, G, =:=, M, N](
      s,
      [A] => fa => f(fa).map { case (t, ne) => Indeed((summon[A =:= A], (t, ne))) }
    ).map:
      case Indeed((TypeEq(Refl()), res)) => res

  def quotedSchemaMotifRelAA[F[_], T, G[_], Rel[_, _], M[_], N[_]](
    s: SchemaMotif[F, T],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    F: AutoTyping[F],
    F1: Faithful[F],
    G: Type[G],
    Rel: Compatible[Rel],
    Iso: ExprIso[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[U] =>> (Rel[T, U], (Type[U], N[Expr[SchemaMotif[G, U]]]))]] =
    s match
      case p: SchemaMotif.Primitive[F, T] =>
        quotedSchemaMotifPrimitiveRefl(p, Rel) match
          case (r, (t, p)) =>
            M.pure(Indeed(r, (t, N.pure(p))))
      case c: SchemaMotif.Constant[F, t] =>
        summon[T =:= Const[t]]
        quotedSchemaMotifConstantRefl(c, Rel) match
          case (r, (t, c)) =>
            given Type[t] = t
            M.pure(Indeed(r.lift_const, (Type.of[Const[t]], N.pure(c))))
      case a: SchemaMotif.Array[s, a] =>
        f(a.elem) map:
          case e @ Indeed((rel, (tb, sb))) =>
            given Type[e.T] = tb
            Exists((rel.lift_arr, (Type.of[Arr[e.T]], sb.map { sb => '{ SchemaMotif.Array($sb) } })))
      case o: SchemaMotif.Object[s, ps] =>
        quotedSchemaMotifObjectRelAA(o.value, f) map:
          case e @ Indeed((rel, (t, s))) =>
            given Type[e.T] = t
            Exists(rel.lift_obj, (Type.of[Obj[e.T]], s.map { s => '{ SchemaMotif.Object($s) } }))
      case o: SchemaMotif.OneOf[s, k, cases] =>
        val (tk, ek) = quotedSingletonString(o.discriminatorProperty)
        given Type[k] = tk
        quotedNamedProductRelAA(o.schemas, [A] => ca => quotedOneOfCaseRelAA[F, k, A, G, Rel, M, N](ca, f))
          .map:
            case ex @ Indeed((rel, (t, nep))) =>
              given Type[ex.T] = t
              Exists((
                rel.lift_discriminatedUnion,
                ( Type.of[DiscriminatedUnion[ex.T]]
                , nep.map { ep => '{ SchemaMotif.OneOf($ek, $ep)} }
                )
              ))

  private def quotedOneOfCaseRelAA[F[_], K <: String, T, G[_], Rel[_, _], M[_], N[_]](
    c: SchemaMotif.OneOf.Case[F, K, T],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    F: AutoTyping[F],
    F1: Faithful[F],
    K: Type[K],
    G: Type[G],
    Iso: ExprIso[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[U] =>> (Rel[T, U], (Type[U], N[Expr[SchemaMotif.OneOf.Case[G, K, U]]]))]] =
    c match
      case ci: SchemaMotif.OneOf.Case.Impl[f, k, ps, v] =>
        summon[T =:= Obj[ps]]
        f(ci.payload).map:
          case ex @ Indeed((rel, (tu, nExpr))) =>
            type U = ex.T
            val nExpr1: N[Expr[G[Obj[ps]]]] =
              nExpr.map: ne =>
                val relG: G[Obj[ps]] `Rel` G[U] =
                  rel.lift[G]
                relG.unapply(ne)
            val nExpr2: N[Expr[SchemaMotif.OneOf.Case[G, k, U]]] =
              nExpr1.map: payloadExpr =>
                val (tk, tps, tv, hasDiscrPropExpr) =
                  quotedIsRequiredPropertyOf(ci.hasDiscriminatorProperty, ci.payload.motif)
                given Type[ps] = tps
                given Type[v] = tv
                val isSingletonStringExpr =
                  quotedSingletonStringSchema[v](ci.isSingletonString)
                    ._2
                val caseExpr: Expr[SchemaMotif.OneOf.Case[G, k, Obj[ps]]] =
                  '{ SchemaMotif.OneOf.Case.Impl($payloadExpr, $hasDiscrPropExpr, $isSingletonStringExpr) }
                rel.lift[SchemaMotif.OneOf.Case[G, k, _]].apply(caseExpr)
            Indeed((rel, (tu, nExpr2)))

  private def quotedSingletonStringSchema[T](s: SingletonStringSchema[T])(using Quotes): (Type[T], Expr[SingletonStringSchema[T]]) =
    s match
      case c: SingletonStringSchema.Constant[s] =>
        val (ts, es) = quotedSingletonString(c.value)
        given Type[s] = ts
        (Type.of[Const[s]], '{ SingletonStringSchema.Constant($es) })
      case enm: SingletonStringSchema.SingletonEnum[s] =>
        val (ts, es) = quotedSingletonString(enm.value)
        given Type[s] = ts
        (Type.of[Enum[Str, Void || s]], '{ SingletonStringSchema.SingletonEnum($es) })

  private def quotedIsRequiredPropertyOf[K <: String, Ps, T, F[_]](
    i: IsRequiredPropertyOf.Aux[K, Ps, T],
    ev: ObjectMotif[F, F, Ps],
  )(using
    Quotes,
    AutoTyping[F],
  ): (Type[K], Type[Ps], Type[T], Expr[IsRequiredPropertyOf.Aux[K, Ps, T]]) =
    i.switchReq[(Type[K], Type[Ps], Type[T], Expr[IsRequiredPropertyOf.Aux[K, Ps, T]])](
      caseLastProp =
        [init] => (ev1: Ps =:= (init || K :: T)) => {
          ev1 match { case TypeEq(Refl()) =>
            val obj: ObjectMotif[F, F, init || K :: T] = ev //ev1.substituteCo(ev)
            val (init, k, ft) = obj.unsnocReq
            given Type[K] = quotedSingletonString(k)._1
            given Type[T] = ft.getType
            given Type[init] = quotedSchemaMotifObject_A[F, init, ConstUnit, ConstUnit](init, [A] => fa => (fa.getType, ()))._1
            ( Type.of[K]
            , Type.of[init || K :: T]
            , Type.of[T]
            , '{ IsPropertyOf.isRequiredLastPropertyOf[init, K, T] }
            )
          }
        },
      caseInitProp =
        [init, last] => (ev1: Ps =:= (init || last), j: IsPropertyOf.Aux[K, init, T, i.ReqOrOpt]) => {
          ev1 match { case TypeEq(Refl()) =>
            val obj: ObjectMotif[F, F, init || last] = ev
            val (init, last) = obj.unsnoc
            val (tk, tinit, tt, eInit) = quotedIsRequiredPropertyOf(j, init)
            given Type[last] = typeOfProperty(last)
            given Type[K] = tk
            given Type[init] = tinit
            given Type[T] = tt
            (tk, Type.of[init || last], tt, '{ IsPropertyOf.isInitPropertyOf[K, init, last](using $eInit) })
          }
        },
    )

  private def typeOfProperty[F[_], P](p: ObjectMotif.Property[F, F, P])(using
    Quotes,
    AutoTyping[F],
  ): Type[P] =
    p match
      case p: ObjectMotif.Property.Required[f, f_, k, t] =>
        given Type[k] = quotedSingletonString(p.name)._1
        given Type[t] = p.value.getType
        Type.of[k :: t]
      case p: ObjectMotif.Property.Optional[f, f_, k, t] =>
        given Type[k] = quotedSingletonString(p.name)._1
        given Type[t] = p.value.getType
        Type.of[k :? t]

  def quotedProduct[F[_], Items](
    p: Items1.Product[||, Void, F, Items],
    f: [A] => F[A] => (Type[A], Expr[F[A]]),
  )(using
    Quotes,
    Type[F],
  ): (Type[Items], Expr[Items1.Product[||, Void, F, Items]]) =
    val g = [A] => (fa: F[A]) => (summon[A =:= A], f(fa))
    quotedProductRefl(p, g, summon[Compatible[=:=]])
      ._2

  def quotedProductRefl[F[_], Items, Rel[_, _]](
    p: Items1.Product[||, Void, F, Items],
    f: [A] => F[A] => (Rel[A, A], (Type[A], Expr[F[A]])),
    Rel: Compatible[Rel],
  )(using
    Quotes,
    Type[F],
  ): (Rel[Items, Items], (Type[Items], Expr[Items1.Product[||, Void, F, Items]])) =
    p match
      case s: Items1.Product.Single[sep, nil, f, a] =>
        summon[Items =:= (Void || a)]
        val (ra, (t, fa)) = f(s.value)
        val rel: (Void || a) `Rel` (Void || a) =
          Rel.lift_||(Rel.lift_void, ra)
        given Type[a] = t
        (rel, (Type.of[Void || a], '{ Items1.Product.Single($fa) }))
      case s: Items1.Product.Snoc[sep, nil, f, init, last] =>
        summon[Items =:= (init || last)]
        val (rLast, (lastTp, lastExp)) = f(s.last)
        val (rInit, (initTp, initExp)) = quotedProductRefl(s.init, f, Rel)
        val rel: (init || last) `Rel` (init || last) =
          Rel.lift_||(rInit, rLast)
        given Type[init] = initTp
        given Type[last] = lastTp
        (rel, (Type.of[init || last], '{ Items1.Product.Snoc($initExp, $lastExp) }))

  def quotedNamedProduct[F[_], Items](
    p: Items1Named.Product[||, ::, F, Items],
    f: [A] => F[A] => (Type[A], Expr[F[A]]),
  )(using
    Quotes,
    Type[F],
  ): (Type[Items], Expr[Items1Named.Product[||, ::, F, Items]]) =
    quotedNamedProductRel[F, Items, F, =:=](p, [A] => fa => Exists((summon, f(fa)))) match
      case Indeed((TypeEq(Refl()), res)) => res

  def quotedNamedProductRel[F[_], Items, G[_], Rel[_, _]](
    p: Items1Named.Product[||, ::, F, Items],
    f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))],
  )(using
    q: Quotes,
    G: Type[G],
    Rel: Compatible[Rel],
  ): Exists[[Elems] =>> (Rel[Items, Elems], (Type[Elems], Expr[Items1Named.Product[||, ::, G, Elems]]))] =
    quotedNamedProductRelAA[F, Items, G, Rel, [x] =>> x, [x] =>> x](p, f)

  def quotedNamedProductUnrelatedAA[F[_], Items, G[_], M[_], N[_]](
    p: Items1Named.Product[||, ::, F, Items],
    f: [A] => F[A] => M[Exists[[B] =>> (Type[B], N[Expr[G[B]]])]],
  )(using
    q: Quotes,
    G: Type[G],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Elems] =>> (Type[Elems], N[Expr[Items1Named.Product[||, ::, G, Elems]]])]] =
    quotedNamedProductRelAA[F, Items, G, Unrelated, M, N](
      p,
      [A] => fa => f(fa).map { case Indeed(te) => Indeed((Unrelated(), te)) }
    ) map {
      case Indeed((_, res)) => Indeed(res)
    }

  def quotedNamedProductRelAA[F[_], Items, G[_], Rel[_, _], M[_], N[_]](
    p: Items1Named.Product[||, ::, F, Items],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    G: Type[G],
    Rel: Compatible[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Elems] =>> (Rel[Items, Elems], (Type[Elems], N[Expr[Items1Named.Product[||, ::, G, Elems]]]))]] =
    p match {
      case s: Items1Named.Product.Single[sep, of, f, lbl, a] =>
        f(s.value) map {
          case ex @ Indeed((rel, (tb, fb))) =>
            type B = ex.T
            given Type[B] = tb

            val (tl, l) = quotedSingletonString(s.label)
            given Type[lbl] = tl

            Indeed((
              rel.lift_-::-[lbl](using s.label),
              (
                Type.of[lbl :: B],
                fb.map { fb =>
                  '{ Items1Named.Product.Single[sep, of, G, lbl, B]($l, $fb) }
                },
              )
            ))
        }
      case s: Items1Named.Product.Snoc[sep, of, f, init, lbl, a] =>
        M.map2(quotedNamedProductRelAA(s.init, f), f(s.lastElem)) {
          case (ex1 @ Indeed((rel1, (tInit, fInit))), ex2 @ Indeed((rel2, (tb, fb)))) =>
            type Elems = ex1.T
            type B = ex2.T
            given Type[Elems] = tInit
            given Type[B] = tb

            val (tl, l) = quotedSingletonString(s.lastName)
            given Type[lbl] = tl

            Indeed((
              Rel.lift_||(rel1, rel2.lift_-::-[lbl](using s.lastName)),
              (
                Type.of[Elems || lbl :: B],
                N.map2(fInit, fb) { (fInit, fb) =>
                  '{ Items1Named.Product.Snoc[sep, of, G, Elems, lbl, B]($fInit, $l, $fb) }
                },
              )
            ))
        }
    }

  def quotedSchemaObject[Ps](s: Schema[Obj[Ps]])(using Quotes): (Type[Ps], Expr[Schema[Obj[Ps]]]) =
    val (tp, expr) = quotedSchemaMotifObject(Schema.asObject(s).value, [A] => sa => quotedSchema(sa))
    given Type[Ps] = tp
    (tp, '{ Schema.Proper[Obj[Ps]](SchemaMotif.Object($expr)) })

  def quotedSchemaMotifObject[F[_], Ps](
    s: ObjectMotif[F, F, Ps],
    f: [A] => F[A] => (Type[A], Expr[F[A]]),
  )(using
    Quotes,
    Type[F],
  ): (Type[Ps], Expr[ObjectMotif[F, F, Ps]]) =
    quotedSchemaMotifObjectRel[F, Ps, F, =:=](s, [A] => fa => Exists((summon[A =:= A], f(fa)))) match
      case Indeed((ev, res)) =>
        ev.substituteContra[[Qs] =>> (Type[Qs], Expr[ObjectMotif[F, F, Qs]])](res)

  def quotedSchemaMotifObjectRel[F[_], Ps, G[_], Rel[_, _]](
    s: ObjectMotif[F, F, Ps],
    f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))],
  )(using
    q: Quotes,
    tg: Type[G],
    Rel: Compatible[Rel],
  ): Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], Expr[ObjectMotif[G, G, Qs]]))] =
    quotedSchemaMotifObjectRelAA[F, Ps, G, Rel, [x] =>> x, [x] =>> x](s, f)

  def quotedSchemaMotifObject_A[F[_], Ps, G[_], N[_]](
    s: ObjectMotif[F, F, Ps],
    f: [A] => F[A] => (Type[A], N[Expr[G[A]]]),
  )(using
    q: Quotes,
    tg: Type[G],
    N: Applicative[N],
  ): (Type[Ps], N[Expr[ObjectMotif[G, G, Ps]]]) =
    quotedSchemaMotifObjectAA[F, Ps, G, [x] =>> x, N](s, f)

  def quotedSchemaMotifObjectAA[F[_], Ps, G[_], M[_], N[_]](
    s: ObjectMotif[F, F, Ps],
    f: [A] => F[A] => M[(Type[A], N[Expr[G[A]]])],
  )(using
    q: Quotes,
    tg: Type[G],
    M: Applicative[M],
    N: Applicative[N],
  ): M[(Type[Ps], N[Expr[ObjectMotif[G, G, Ps]]])] =
    quotedSchemaMotifObjectRelAA[F, Ps, G, =:=, M, N](
      s,
      [A] => fa => f(fa).map(te => Indeed((summon[A =:= A], te)))
    ).map:
      case Indeed((TypeEq(Refl()), res)) => res

  def quotedSchemaMotifObjectRelAA[F[_], Ps, G[_], Rel[_, _], M[_], N[_]](
    s: ObjectMotif[F, F, Ps],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    tg: Type[G],
    Rel: Compatible[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], N[Expr[ObjectMotif[G, G, Qs]]]))]] =
    s match
      case ObjectMotif.Empty() =>
        M.pure(
          Exists(Rel.lift_void, (Type.of[Void], N.pure('{ ObjectMotif.Empty() })))
        )
      case ne: ObjectMotif.Snoc[f, f_, init, p] =>
        M.map2(
          quotedSchemaMotifObjectRelAA(ne.init, f),
          quotedObjectPropertyRelAA(ne.last, f),
        ):
          case (ex1 @ Indeed((ri, (tqs, init))), ex2 @ Indeed((pRq, tq, q))) =>
            type QInit = ex1.T
            given Type[QInit] = tqs
            type Q = ex2.T
            given Type[Q] = tq
            Indeed((Rel.lift_||(ri, pRq), (Type.of[QInit || Q], N.map2(init, q) { (init, q) => '{ ObjectMotif.Snoc($init, $q) } })))

  private def quotedObjectPropertyRelAA[F[_], P, G[_], Rel[_, _], M[_], N[_]](
    p: ObjectMotif.Property[F, F, P],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    G: Type[G],
    Rel: Compatible[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Q] =>> (
    Rel[P, Q],
    Type[Q],
    N[Expr[ObjectMotif.Property[G, G, Q]]],
  )]] =
    p match
      case req @ ObjectMotif.Property.Required(_, _) =>
        quotedObjectPropertyReqRelAA(req, f)
      case opt @ ObjectMotif.Property.Optional(_, _) =>
        quotedObjectPropertyOptRelAA(opt, f)

  private def quotedObjectPropertyReqRelAA[F[_], PropName <: String, PropType, G[_], Rel[_, _], M[_], N[_]](
    p: ObjectMotif.Property.Required[F, F, PropName, PropType],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    G: Type[G],
    Rel: Compatible[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Q] =>> (
    Rel[PropName :: PropType, Q],
    Type[Q],
    N[Expr[ObjectMotif.Property[G, G, Q]]],
  )]] =
    f(p.value).map {
      case ex @ Indeed((rl, (tl, sl))) =>
        type B  = ex.T
        given Type[B] = tl
        val (nt, spn) = quotedSingletonString(p.name)
        given Type[PropName] = nt

        val expr: N[Expr[ObjectMotif.Property[G, G, PropName :: B]]] =
          sl.map: sl =>
            '{ ObjectMotif.Property.Required[G, G, PropName, B]($spn, $sl) }

        val pRq: (PropName :: PropType) `Rel` (PropName :: B) =
          rl.lift_-::-[PropName](using p.name)

        Exists((
          pRq,
          Type.of[PropName :: B],
          expr,
        ))
    }

  private def quotedObjectPropertyOptRelAA[F[_], PropName <: String, PropType, G[_], Rel[_, _], M[_], N[_]](
    p: ObjectMotif.Property.Optional[F, F, PropName, PropType],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    G: Type[G],
    Rel: Compatible[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Q] =>> (
    Rel[PropName :? PropType, Q],
    Type[Q],
    N[Expr[ObjectMotif.Property[G, G, Q]]],
  )]] =
    f(p.value).map {
      case ex @ Indeed((rl, (tl, sl))) =>
        type B  = ex.T
        given Type[B] = tl
        val (nt, spn) = quotedSingletonString(p.name)
        given Type[PropName] = nt

        val expr: N[Expr[ObjectMotif.Property[G, G, PropName :? B]]] =
          sl.map: sl =>
            '{ ObjectMotif.Property.Optional[G, G, PropName, B]($spn, $sl) }

        val pRq: (PropName :? PropType) `Rel` (PropName :? B) =
          rl.lift_-:?-[PropName](using p.name)

        Exists((
          pRq,
          Type.of[PropName :? B],
          expr,
        ))
    }

  private def prodSingle[F[_], Label <: String, T](
    label: SingletonType[Label],
    value: F[T],
  ): Items1Named.Product[||, ::, F, Label :: T] =
    Items1Named.Product.Single(label, value)

  def quotedSingletonString[T <: String](x: SingletonType[T])(using
    Quotes,
  ): (Type[T], Expr[SingletonType[T]]) =
    quotedSingletonType(x, quotedStringLiteral)

  def quotedSingletonInt[T <: Int](x: SingletonType[T])(using
    Quotes,
  ): (Type[T], Expr[SingletonType[T]]) =
    quotedSingletonType(x, quotedIntLiteral)

  def quotedSingletonLong[T <: Long](x: SingletonType[T])(using
    Quotes,
  ): (Type[T], Expr[SingletonType[T]]) =
    quotedSingletonType(x, quotedLongLiteral)

  def quotedSingletonBoolean[T <: Boolean](x: SingletonType[T])(using
    Quotes,
  ): (Type[T], Expr[SingletonType[T]]) =
    quotedSingletonType(x, quotedBooleanLiteral)

  given stringSingletonToExpr[T <: String]: ToExpr[SingletonType[T]] with {
    override def apply(x: SingletonType[T])(using Quotes): Expr[SingletonType[T]] =
      quotedSingletonString(x)._2
  }

  def quotedSingletonType[Base, T <: Base](
    x: SingletonType[T],
    quoteSingleton: (a: Base) => (Type[a.type], Expr[a.type]),
  )(using
    Quotes,
  ): (Type[T], Expr[SingletonType[T]]) = {
    import quotes.reflect.*

    val (tpe, trm) = quoteSingleton(x.value)
    given Type[T] = x.witness.substituteContra(tpe)

    (
      Type.of[T],
      '{ SingletonType($trm) }.asExprOf[SingletonType[T]],
    )
  }

  private def quotedStringLiteral(s: String)(using Quotes): (Type[s.type], Expr[s.type]) = {
    import quotes.reflect.*

    val term = Literal(StringConstant(s))
    val tpe = term.tpe.asType.asInstanceOf[Type[s.type]]
    (tpe, term.asExprOf(using tpe))
  }

  private def quotedIntLiteral(i: Int)(using Quotes): (Type[i.type], Expr[i.type]) = {
    import quotes.reflect.*

    val term = Literal(IntConstant(i))
    val tpe = term.tpe.asType.asInstanceOf[Type[i.type]]
    (tpe, term.asExprOf(using tpe))
  }

  private def quotedLongLiteral(l: Long)(using Quotes): (Type[l.type], Expr[l.type]) = {
    import quotes.reflect.*

    val term = Literal(LongConstant(l))
    val tpe = term.tpe.asType.asInstanceOf[Type[l.type]]
    (tpe, term.asExprOf(using tpe))
  }

  private def quotedBooleanLiteral(b: Boolean)(using Quotes): (Type[b.type], Expr[b.type]) = {
    import quotes.reflect.*

    val term = Literal(BooleanConstant(b))
    val tpe = term.tpe.asType.asInstanceOf[Type[b.type]]
    (tpe, term.asExprOf(using tpe))
  }
}
