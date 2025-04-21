package jing.openapi

import jing.openapi.model.*
import scala.quoted.*
import libretto.lambda.Items1Named
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
    val HttpEndpoint(path, meth, req, resp) = x
    val (reqType, reqExpr) = quotedRequestSchema(req)
    val (respType, respExpr) = quotedResponseSchema(resp)

    given Type[Is] = reqType
    given Type[O] = respType

    (
      Type.of[HttpEndpoint[Is, O]],
      '{ HttpEndpoint(${Expr(path)}, ${Expr(meth)}, ${reqExpr}, ${respExpr}) },
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
        val (t, ps) = quotedParamsNonEmpty(p.params)
        given Type[ps] = t
        (Type.of[Void || "params" :: Obj[ps]], '{ RequestSchema.Parameterized($ps) })

  def quotedParamsNonEmpty[Ps](
    ps: RequestSchema.Params.NonEmpty[Ps],
  )(using
    Quotes,
  ): (Type[Ps], Expr[RequestSchema.Params.NonEmpty[Ps]]) =
    ps match
      case RequestSchema.Params.NonEmpty(path, schema) =>
        val (t, s) = quotedSchemaObjectNonEmpty(schema)
        given Type[Ps] = t
        (t, '{ RequestSchema.Params.NonEmpty(${Expr(path)}, $s) })


  def quotedResponseSchema[T](
    x: ResponseSchema[T],
  )(using
    Quotes,
  ): (Type[T], Expr[ResponseSchema[T]]) =
    x match
      case rs: ResponseSchema.ByStatusCode[as] =>
        val (t, e) =
          quotedProduct(
            rs.items,
            [A] => bs => quotedBodySchema(bs),
          )
        given Type[as] = t
        (Type.of[DiscriminatedUnion[as]], '{ ResponseSchema.ByStatusCode($e) })

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
          quotedProduct(
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

  def quotedSchemaFromProto[F[_]](
    schema: ProtoSchema.Oriented,
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Reader[SchemaLookup[F], Exists[[T] =>> (Type[T], F[Expr[Schema[T]]])]] = {
    import quotes.reflect.*

    schema match
      case ProtoSchema.Oriented.Proper(value) =>
        quotedSchemaMotifRelAA(
          value,
          [A] => ps => quotedSchemaFromProto(ps) map {
            case Indeed(te) => Indeed((Unrelated(), te))
          },
        ) map {
          case ex @ Indeed(_, (tpe, expr)) =>
            given Type[ex.T] = tpe
            Indeed((tpe, expr.map { expr => '{ Schema.Proper($expr) } }))
          }

      case ProtoSchema.Oriented.BackwardRef(schemaName) =>
        Reader(_.lookup(schemaName))

      case ProtoSchema.Oriented.ForwardRef(schemaName, cycle) =>
        val msg = s"Unsupported recursive schema: ${cycle.mkString(" -> ")}"
        val (tpe, trm) = quotedSchemaOops(SingletonType(msg))
        Reader.pure( Exists(tpe, F.pure(trm)) )

      case ProtoSchema.Oriented.UnresolvedRef(schemaName) =>
        val msg = s"Unresolved schema $schemaName"
        val (tpe, trm) = quotedSchemaOops(SingletonType(msg))
        Reader.pure( Exists(tpe, F.pure(trm)) )

      case ProtoSchema.Oriented.Unsupported(details) =>
        val (tpe, trm) = quotedSchemaOops(SingletonType(details))
        Reader.pure( Exists((tpe, F.pure(trm))) )
  }

  def quotedSchemaObjectFromProto[F[_]](
    schema: SchemaMotif.Object[[x] =>> ProtoSchema.Oriented, ?],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Reader[SchemaLookup[F], Exists[[Ps] =>> (Type[Ps], F[Expr[Schema[Obj[Ps]]]])]] =
    quotedSchemaMotifObjectRelAA(
      schema,
      [A] => ps => quotedSchemaFromProto(ps) map {
        case Indeed(te) => Indeed((Unrelated(), te))
      }
    ) map {
      case ex @ Indeed(_, (tpe, expr)) =>
        given Type[ex.T] = tpe
        Indeed((tpe, expr.map { expr => '{ Schema.Proper($expr) } }))
    }

  def quotedSchemaObjectNonEmptyFromProto[F[_]](
    schema: SchemaMotif.Object.NonEmpty[[x] =>> ProtoSchema.Oriented, ?],
  )(using
    q: Quotes,
    F: Applicative[F],
  ): Reader[SchemaLookup[F], Exists[[Ps] =>> (Type[Ps], F[Expr[Schema.Object.NonEmpty[Ps]]])]] =
    quotedSchemaMotifObjectNonEmptyRelAA(
      schema,
      [A] => ps => quotedSchemaFromProto(ps) map {
        case Indeed(te) => Indeed((Unrelated(), te))
      }
    ) map {
      case ex @ Indeed(_, (tpe, expr)) =>
        given Type[ex.T] = tpe
        Indeed((tpe, expr.map { expr => '{ Schema.Object.NonEmpty.fromMotif($expr) } }))
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
      case SchemaMotif.I32() =>
        M.pure(
          Exists((Rel.refl[Int32], (Type.of[Int32], N.pure('{ SchemaMotif.I32() }))))
        )
      case SchemaMotif.I64() =>
        M.pure(
          Exists((Rel.refl[Int64], (Type.of[Int64], N.pure('{ SchemaMotif.I64() }))))
        )
      case SchemaMotif.S() =>
        M.pure(
          Exists((Rel.refl[Str], (Type.of[Str], N.pure('{ SchemaMotif.S() }))))
        )
      case SchemaMotif.B() =>
        M.pure(
          Exists((Rel.refl[Bool], (Type.of[Bool], N.pure('{ SchemaMotif.B() }))))
        )
      case a: SchemaMotif.Array[s, a] =>
        f(a.elem) map:
          case e @ Indeed((rel, (tb, sb))) =>
            given Type[e.T] = tb
            Exists((rel.lift[Arr], (Type.of[Arr[e.T]], sb.map { sb => '{ SchemaMotif.Array($sb) } })))
      case o: SchemaMotif.Object[s, ps] =>
        quotedSchemaMotifObjectRelAA(o, f) map:
          case e @ Indeed((rel, (t, s))) =>
            given Type[e.T] = t
            Exists(rel.lift[Obj], (Type.of[Obj[e.T]], s.widen))

  def quotedProduct[F[_], Items](
    p: Items1Named.Product[||, ::, F, Items],
    f: [A] => F[A] => (Type[A], Expr[F[A]]),
  )(using
    Quotes,
    Type[F],
  ): (Type[Items], Expr[Items1Named.Product[||, ::, F, Items]]) =
    quotedProductRel[F, Items, F, =:=](p, [A] => fa => Exists((summon, f(fa)))) match
      case Indeed((TypeEq(Refl()), res)) => res

  def quotedProductRel[F[_], Items, G[_], Rel[_, _]](
    p: Items1Named.Product[||, ::, F, Items],
    f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))],
  )(using
    q: Quotes,
    G: Type[G],
    Rel: Substitutive[Rel],
  ): Exists[[Elems] =>> (Rel[Items, Elems], (Type[Elems], Expr[Items1Named.Product[||, ::, G, Elems]]))] =
    quotedProductRelAA[F, Items, G, Rel, [x] =>> x, [x] =>> x](p, f)

  def quotedProductUnrelatedAA[F[_], Items, G[_], M[_], N[_]](
    p: Items1Named.Product[||, ::, F, Items],
    f: [A] => F[A] => M[Exists[[B] =>> (Type[B], N[Expr[G[B]]])]],
  )(using
    q: Quotes,
    G: Type[G],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Elems] =>> (Type[Elems], N[Expr[Items1Named.Product[||, ::, G, Elems]]])]] =
    quotedProductRelAA[F, Items, G, Unrelated, M, N](
      p,
      [A] => fa => f(fa).map { case Indeed(te) => Indeed((Unrelated(), te)) }
    ) map {
      case Indeed((_, res)) => Indeed(res)
    }

  def quotedProductRelAA[F[_], Items, G[_], Rel[_, _], M[_], N[_]](
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
        M.map2(quotedProductRelAA(s.init, f), f(s.lastElem)) {
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
    val (tp, expr) = quotedSchemaMotifObject(Schema.asObject(s), [A] => sa => quotedSchema(sa))
    given Type[Ps] = tp
    (tp, '{ Schema.Proper[Obj[Ps]]($expr) })

  def quotedSchemaObjectNonEmpty[Ps](s: Schema.Object.NonEmpty[Ps])(using Quotes): (Type[Ps], Expr[Schema.Object.NonEmpty[Ps]]) =
    val (tp, expr) = quotedSchemaMotifObjectNonEmpty(s.toMotif, [A] => sa => quotedSchema(sa))
    given Type[Ps] = tp
    (tp, '{ Schema.Object.NonEmpty.fromMotif($expr) })

  def quotedSchemaMotifObject[F[_], Ps](
    s: SchemaMotif.Object[F, Ps],
    f: [A] => F[A] => (Type[A], Expr[F[A]]),
  )(using
    Quotes,
    Type[F],
  ): (Type[Ps], Expr[SchemaMotif.Object[F, Ps]]) =
    quotedSchemaMotifObjectRel[F, Ps, F, =:=](s, [A] => fa => Exists((summon[A =:= A], f(fa)))) match
      case Indeed((ev, res)) =>
        ev.substituteContra[[Qs] =>> (Type[Qs], Expr[SchemaMotif.Object[F, Qs]])](res)

  def quotedSchemaMotifObjectNonEmpty[F[_], Ps](
    s: SchemaMotif.Object.NonEmpty[F, Ps],
    f: [A] => F[A] => (Type[A], Expr[F[A]]),
  )(using
    Quotes,
    Type[F],
  ): (Type[Ps], Expr[SchemaMotif.Object.NonEmpty[F, Ps]]) =
    quotedSchemaMotifObjectNonEmptyRel[F, Ps, F, =:=](s, [A] => fa => Exists((summon[A =:= A], f(fa)))) match
      case Indeed((ev, res)) =>
        ev.substituteContra[[Qs] =>> (Type[Qs], Expr[SchemaMotif.Object.NonEmpty[F, Qs]])](res)

  def quotedSchemaMotifObjectRel[F[_], Ps, G[_], Rel[_, _]](
    s: SchemaMotif.Object[F, Ps],
    f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))],
  )(using
    q: Quotes,
    tg: Type[G],
    Rel: Substitutive[Rel],
  ): Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], Expr[SchemaMotif.Object[G, Qs]]))] =
    quotedSchemaMotifObjectRelAA[F, Ps, G, Rel, [x] =>> x, [x] =>> x](s, f)

  def quotedSchemaMotifObjectNonEmptyRel[F[_], Ps, G[_], Rel[_, _]](
    s: SchemaMotif.Object.NonEmpty[F, Ps],
    f: [A] => F[A] => Exists[[B] =>> (Rel[A, B], (Type[B], Expr[G[B]]))],
  )(using
    q: Quotes,
    tg: Type[G],
    Rel: Substitutive[Rel],
  ): Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], Expr[SchemaMotif.Object.NonEmpty[G, Qs]]))] =
    quotedSchemaMotifObjectNonEmptyRelAA[F, Ps, G, Rel, [x] =>> x, [x] =>> x](s, f)

  def quotedSchemaMotifObjectRelAA[F[_], Ps, G[_], Rel[_, _], M[_], N[_]](
    s: SchemaMotif.Object[F, Ps],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    tg: Type[G],
    Rel: Substitutive[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], N[Expr[SchemaMotif.Object[G, Qs]]]))]] =
    s match
      case SchemaMotif.Object.Empty() =>
        M.pure(
          Exists(Rel.refl[Void], (Type.of[Void], N.pure('{ SchemaMotif.Object.Empty() })))
        )
      case ne: SchemaMotif.Object.NonEmpty[f, ps] =>
        quotedSchemaMotifObjectNonEmptyRelAA(ne, f).map:
          case Indeed((rel, (tp, s))) => Indeed((rel, (tp, s.widen)))

  def quotedSchemaMotifObjectNonEmptyRelAA[F[_], Ps, G[_], Rel[_, _], M[_], N[_]](
    s: SchemaMotif.Object.NonEmpty[F, Ps],
    f: [A] => F[A] => M[Exists[[B] =>> (Rel[A, B], (Type[B], N[Expr[G[B]]]))]],
  )(using
    q: Quotes,
    tg: Type[G],
    Rel: Substitutive[Rel],
    M: Applicative[M],
    N: Applicative[N],
  ): M[Exists[[Qs] =>> (Rel[Ps, Qs], (Type[Qs], N[Expr[SchemaMotif.Object.NonEmpty[G, Qs]]]))]] =
    s match
      case snoc @ SchemaMotif.Object.Snoc(init, pname, ptype) =>
        quotedSchemaMotifObjectSnocRelAA(snoc, f)
      case snoc @ SchemaMotif.Object.SnocOpt(init, pname, ptype) =>
        quotedSchemaMotifObjectSnocOptRelAA(snoc, f)

  private def quotedSchemaMotifObjectSnocRelAA[F[_], Init, PropName <: String, PropType, G[_], Rel[_, _], M[_], N[_]](
    snoc: SchemaMotif.Object.Snoc[F, Init, PropName, PropType],
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
      N[Expr[SchemaMotif.Object.NonEmpty[G, Qs]]],
    )
  )]] = {
    M.map2(
      quotedSchemaMotifObjectRelAA(snoc.init, f),
      f(snoc.ptype),
    ) {
      case (e1 @ Indeed((ri, (ti, si))), e2 @ Indeed((rl, (tl, sl)))) =>
        type As = e1.T
        type B  = e2.T
        given Type[As] = ti
        given Type[B] = tl

        val (nt, spn) = quotedSingletonString(snoc.pname)

        given Type[PropName] = nt

        val expr: N[Expr[SchemaMotif.Object.NonEmpty[G, As || PropName :: B]]] =
          N.map2(si, sl) { (si, sl) =>
            '{ SchemaMotif.Object.Snoc[G, As, PropName, B]($si, $spn, $sl) }
          }

        Exists((
          Rel.biLift(ri, rl)[[X, Y] =>> X || PropName :: Y],
          (Type.of[As || PropName :: B], expr)
        ))
    }
  }

  private def quotedSchemaMotifObjectSnocOptRelAA[F[_], Init, PropName <: String, PropType, G[_], Rel[_, _], M[_], N[_]](
    snoc: SchemaMotif.Object.SnocOpt[F, Init, PropName, PropType],
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
      N[Expr[SchemaMotif.Object.NonEmpty[G, Qs]]],
    )
  )]] = {
    M.map2(
      quotedSchemaMotifObjectRelAA(snoc.init, f),
      f(snoc.ptype),
    ) {
      case (e1 @ Indeed((ri, (ti, si))), e2 @ Indeed((rl, (tl, sl)))) =>
        type As = e1.T
        type B  = e2.T
        given Type[As] = ti
        given Type[B] = tl

        val (nt, spn) = quotedSingletonString(snoc.pname)

        given Type[PropName] = nt

        val expr: N[Expr[SchemaMotif.Object.NonEmpty[G, As || PropName :? B]]] =
          N.map2(si, sl) { (si, sl) =>
            '{ SchemaMotif.Object.SnocOpt[G, As, PropName, B]($si, $spn, $sl) }
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

  private def quotedSingletonString[T <: String](x: SingletonType[T])(using
    Quotes,
  ): (Type[T], Expr[SingletonType[T]]) = {
    import quotes.reflect.*

    val (tpe, trm) = quotedStringLiteral(x.value)
    given Type[T] = x.witness.substituteContra(tpe)

    (
      Type.of[T],
      '{ SingletonType($trm) }.asExprOf[SingletonType[T]],
    )
  }

  given stringSingletonToExpr[T <: String]: ToExpr[SingletonType[T]] with {
    override def apply(x: SingletonType[T])(using Quotes): Expr[SingletonType[T]] =
      quotedSingletonString(x)._2
  }

  private def quotedStringLiteral(s: String)(using Quotes): (Type[s.type], Expr[s.type]) = {
    import quotes.reflect.*

    val term = Literal(StringConstant(s))
    val tpe = term.tpe.asType.asInstanceOf[Type[s.type]]
    (tpe, term.asExprOf(using tpe))
}
}
