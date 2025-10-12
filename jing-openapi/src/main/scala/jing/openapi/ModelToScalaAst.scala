package jing.openapi

import jing.openapi.model.*
import scala.quoted.*
import libretto.lambda.{Items1, Items1Named}
import libretto.lambda.util.{Applicative, Exists, SingletonType, TypeEq}
import libretto.lambda.util.Applicative.*
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq.Refl

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
    s match
      case Schema.Proper(s) =>
        val (tpe, exp) = quotedSchemaMotif(s, [A] => sa => quotedSchema(sa))
        given Type[T] = tpe
        (tpe, '{ Schema.Proper($exp) })
      case u: Schema.Unsupported[msg] =>
        quotedSchemaOops(u.message)

  private def quotedSchemaOops[S <: String](s: SingletonType[S])(using Quotes): (Type[Oops[S]], Expr[Schema[Oops[S]]]) =
    val (tpe, exp) = quotedSingletonString(s)
    given Type[S] = tpe
    (Type.of[Oops[S]], '{ Schema.unsupported($exp) })

  /** Quotes a [[Labeled.Schema]] as `Expr[Schema[?]]`, substituting each labeled schema by a lookup in [[SchemaLookup]].
   *
   * Since the looked up type may potentially be different from the one next to the label, the result type is existential.
   */
  def quotedSchemaWithReferences[F[_]](
    schema: Schema.Labeled[String, ?],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Reader[SchemaLookup[F], Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])]] = {
    import quotes.reflect.*

    schema match
      case Schema.Labeled.Unlabeled(value) =>
        quotedSchemaMotifRelAA(
          value,
          [A] => ps => quotedSchemaWithReferences(ps) map {
            case Indeed(te) => Indeed((Unrelated(), te))
          },
        ) map {
          case ex @ Indeed(_, (tpe, expr)) =>
            given Type[ex.T] = tpe
            Indeed((tpe, expr.map { expr => '{ Schema.Proper($expr) } }))
          }

      case Schema.Labeled.WithLabel(schemaName, _) =>
        Reader(_.lookup(schemaName))

      case Schema.Labeled.Unsupported(details) =>
        val (tpe, trm) = quotedSchemaOops(details)
        Reader.pure( Exists((tpe, F.pure(trm))) )
  }

  def quotedSchemaObjectWithReferences[F[_]](
    schema: SchemaMotif.Object[Schema.Labeled[String, _], ?],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Reader[SchemaLookup[F], Exists[[Ps] =>> (Type[Ps], F[Expr[Schema[Obj[Ps]]]])]] =
    quotedSchemaMotifObjectRelAA(
      schema.asObject.value,
      [A] => ps => quotedSchemaWithReferences(ps) map {
        case Indeed(te) => Indeed((Unrelated(), te))
      }
    ) map {
      case ex @ Indeed(_, (tpe, expr)) =>
        given Type[ex.T] = tpe
        Indeed((tpe, expr.map { expr => '{ Schema.Proper(SchemaMotif.Object($expr)) } }))
    }

  def quotedScalaValueOf[T, U](v: ScalaValueOf[T, U])(using Quotes, Type[U]): (Type[T], Expr[ScalaValueOf[T, U]]) = {
    import ScalaValueOf.*

    v match
      case I32(i) =>
        summon[T <:< Int]
        val (tp, exp) = quotedSingletonInt(i)
        given Type[T] = tp
        (tp, '{ I32($exp) })
      case I64(l) =>
        summon[T <:< Long]
        val (tp, exp) = quotedSingletonLong(l)
        given Type[T] = tp
        (tp, '{ I64($exp) })
      case S(s) =>
        summon[T <:< String]
        val (tp, exp) = quotedSingletonString(s)
        given Type[T] = tp
        (tp, '{ S($exp) })
      case B(b) =>
        summon[T <:< Boolean]
        val (tp, exp) = quotedSingletonBoolean(b)
        given Type[T] = tp
        (tp, '{ B($exp) })
  }

  def quotedSchemaMotifBasicPrimitive[F[_], T, G[_]](
    s: SchemaMotif.BasicPrimitive[F, T],
  )(using
    Quotes,
    Type[G],
  ): (Type[T], Expr[SchemaMotif.BasicPrimitive[G, T]]) = {
    import jing.openapi.model.SchemaMotif.*

    s match
      case I32() => (Type.of[Int32], '{ I32() })
      case I64() => (Type.of[Int64], '{ I64() })
      case S()   => (Type.of[Str], '{ S() })
      case B()   => (Type.of[Bool], '{ B() })
  }

  def quotedSchemaMotifPrimitive[F[_], T, G[_]](
    s: SchemaMotif.Primitive[F, T],
  )(using
    Quotes,
    Type[G],
  ): (Type[T], Expr[SchemaMotif.Primitive[G, T]]) = {
    import jing.openapi.model.SchemaMotif.{BasicPrimitive, Enumeration}

    s match
      case b: BasicPrimitive[F, T] =>
        quotedSchemaMotifBasicPrimitive(b)
      case e: Enumeration[F, base, cases] =>
        quotedSchemaMotifBasicPrimitive(e.baseType) match
          case (tb, b) =>
            given Type[base] = tb
            quotedProduct(
              e.cases,
              [A] => (v: ScalaValueOf[A, base]) => quotedScalaValueOf(v)
            ) match
              case (tc, cs) =>
                given Type[cases] = tc
                (Type.of[Enum[base, cases]], '{ Enumeration[G, base, cases](${b}, ${cs})})
  }

  def quotedSchemaMotif[F[_], T](
    s: SchemaMotif[F, T],
    f: [A] => F[A] => (Type[A], Expr[F[A]]),
  )(using
    Quotes,
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
    Type[G],
    Substitutive[Rel],
  ): Exists[[U] =>> (Rel[T, U], (Type[U], Expr[SchemaMotif[G, U]]))] =
    quotedSchemaMotifRelAA[F, T, G, Rel, [x] =>> x, [x] =>> x](s, f)

  def quotedSchemaMotifRelAA[F[_], T, G[_], Rel[_, _], M[_], N[_]](
    s: SchemaMotif[F, T],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    G: Type[G],
    Rel: Substitutive[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[U] =>> (Rel[T, U], (Type[U], N[Expr[SchemaMotif[G, U]]]))]] =
    s match
      case p: SchemaMotif.Primitive[F, T] =>
        quotedSchemaMotifPrimitive(p) match
          case (t, p) =>
            M.pure(Indeed(Rel.refl[T], (t, N.pure(p))))
      case a: SchemaMotif.Array[s, a] =>
        f(a.elem) map:
          case e @ Indeed((rel, (tb, sb))) =>
            given Type[e.T] = tb
            Exists((rel.lift[Arr], (Type.of[Arr[e.T]], sb.map { sb => '{ SchemaMotif.Array($sb) } })))
      case o: SchemaMotif.Object[s, ps] =>
        quotedSchemaMotifObjectRelAA(o.value, f) map:
          case e @ Indeed((rel, (t, s))) =>
            given Type[e.T] = t
            Exists(rel.lift[Obj], (Type.of[Obj[e.T]], s.map { s => '{ SchemaMotif.Object($s) } }))
      case o: SchemaMotif.OneOf[s, cases] =>
        quotedNamedProductRelAA(o.schemas, f)
          .map:
            case ex @ Indeed((rel, (t, nep))) =>
              given Type[ex.T] = t
              Exists((
                rel.lift[DiscriminatedUnion],
                ( Type.of[DiscriminatedUnion[ex.T]]
                , nep.map { ep => '{ SchemaMotif.OneOf(${Expr(o.discriminatorProperty)}, $ep)} }
                )
              ))

  def quotedProduct[F[_], Items](
    p: Items1.Product[||, Void, F, Items],
    f: [A] => F[A] => (Type[A], Expr[F[A]]),
  )(using
    Quotes,
    Type[F],
  ): (Type[Items], Expr[Items1.Product[||, Void, F, Items]]) =
    p match
      case s: Items1.Product.Single[sep, nil, f, a] =>
        summon[Items =:= (Void || a)]
        val (t, fa) = f(s.value)
        given Type[a] = t
        (Type.of[Void || a], '{ Items1.Product.Single($fa) })
      case s: Items1.Product.Snoc[sep, nil, f, init, last] =>
        summon[Items =:= (init || last)]
        val (initTp, initExp) = quotedProduct(s.init, f)
        val (lastTp, lastExp) = f(s.last)
        given Type[init] = initTp
        given Type[last] = lastTp
        (Type.of[init || last], '{ Items1.Product.Snoc($initExp, $lastExp) })

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
    Rel: Substitutive[Rel],
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
    Rel: Substitutive[Rel],
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
              rel.lift[[x] =>> lbl :: x],
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
              Rel.biLift(rel1, rel2)[[X, Y] =>> X || lbl :: Y],
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
    Rel: Substitutive[Rel],
  ): Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], Expr[ObjectMotif[G, G, Qs]]))] =
    quotedSchemaMotifObjectRelAA[F, Ps, G, Rel, [x] =>> x, [x] =>> x](s, f)

  def quotedSchemaMotifObjectRelAA[F[_], Ps, G[_], Rel[_, _], M[_], N[_]](
    s: ObjectMotif[F, F, Ps],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    tg: Type[G],
    Rel: Substitutive[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], N[Expr[ObjectMotif[G, G, Qs]]]))]] =
    s match
      case ObjectMotif.Empty() =>
        M.pure(
          Exists(Rel.refl[Void], (Type.of[Void], N.pure('{ ObjectMotif.Empty() })))
        )
      case ne: ObjectMotif.NonEmpty[f, f_, ps] =>
        quotedSchemaMotifObjectNonEmptyRelAA(ne, f).map:
          case Indeed((rel, (tp, s))) => Indeed((rel, (tp, s.widen)))

  def quotedSchemaMotifObjectNonEmptyRelAA[F[_], Ps, G[_], Rel[_, _], M[_], N[_]](
    s: ObjectMotif.NonEmpty[F, F, Ps],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    tg: Type[G],
    Rel: Substitutive[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], N[Expr[ObjectMotif.NonEmpty[G, G, Qs]]]))]] =
    s match
      case snoc @ ObjectMotif.Snoc(init, pname, ptype) =>
        quotedSchemaMotifObjectSnocRelAA(snoc, f)
      case snoc @ ObjectMotif.SnocOpt(init, pname, ptype) =>
        quotedSchemaMotifObjectSnocOptRelAA(snoc, f)

  private def quotedSchemaMotifObjectSnocRelAA[F[_], Init, PropName <: String, PropType, G[_], Rel[_, _], M[_], N[_]](
    snoc: ObjectMotif.Snoc[F, F, Init, PropName, PropType],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    G: Type[G],
    Rel: Substitutive[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Qs] =>> (
    Rel[Init || PropName :: PropType, Qs],
    (
      Type[Qs],
      N[Expr[ObjectMotif.NonEmpty[G, G, Qs]]],
    )
  )]] = {
    M.map2(
      quotedSchemaMotifObjectRelAA(snoc.init, f),
      f(snoc.pval),
    ) {
      case (e1 @ Indeed((ri, (ti, si))), e2 @ Indeed((rl, (tl, sl)))) =>
        type As = e1.T
        type B  = e2.T
        given Type[As] = ti
        given Type[B] = tl

        val (nt, spn) = quotedSingletonString(snoc.pname)

        given Type[PropName] = nt

        val expr: N[Expr[ObjectMotif.NonEmpty[G, G, As || PropName :: B]]] =
          N.map2(si, sl) { (si, sl) =>
            '{ ObjectMotif.Snoc[G, G, As, PropName, B]($si, $spn, $sl) }
          }

        Exists((
          Rel.biLift(ri, rl)[[X, Y] =>> X || PropName :: Y],
          (Type.of[As || PropName :: B], expr)
        ))
    }
  }

  private def quotedSchemaMotifObjectSnocOptRelAA[F[_], Init, PropName <: String, PropType, G[_], Rel[_, _], M[_], N[_]](
    snoc: ObjectMotif.SnocOpt[F, F, Init, PropName, PropType],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    G: Type[G],
    Rel: Substitutive[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Qs] =>> (
    Rel[Init || PropName :? PropType, Qs],
    (
      Type[Qs],
      N[Expr[ObjectMotif.NonEmpty[G, G, Qs]]],
    )
  )]] = {
    M.map2(
      quotedSchemaMotifObjectRelAA(snoc.init, f),
      f(snoc.pval),
    ) {
      case (e1 @ Indeed((ri, (ti, si))), e2 @ Indeed((rl, (tl, sl)))) =>
        type As = e1.T
        type B  = e2.T
        given Type[As] = ti
        given Type[B] = tl

        val (nt, spn) = quotedSingletonString(snoc.pname)

        given Type[PropName] = nt

        val expr: N[Expr[ObjectMotif.NonEmpty[G, G, As || PropName :? B]]] =
          N.map2(si, sl) { (si, sl) =>
            '{ ObjectMotif.SnocOpt[G, G, As, PropName, B]($si, $spn, $sl) }
          }

        Exists((
          Rel.biLift(ri, rl)[[X, Y] =>> X || PropName :? Y],
          (Type.of[As || PropName :? B], expr)
        ))
    }
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
