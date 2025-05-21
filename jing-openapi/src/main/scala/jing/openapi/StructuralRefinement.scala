package jing.openapi

import jing.openapi.Mode.IsSubsumedBy
import jing.openapi.Mode.IsSubsumedBy.given
import libretto.lambda.util.TypeEq
import libretto.lambda.util.TypeEq.Refl
import scala.quoted.*

private object StructuralRefinement {

  /** Returns a structurally typed term, i.e.
   *
   * ```scala
   * new Base with Selectable { <refinements> } : Base & Selectable { <refinements> }
   * ```
   */
  def typedTerm[Base](using baseType: Type[Base])[Q <: Quotes & Singleton](using q: Q)(
    owner : qr.Symbol,
    members : List[(String, MemberDef.Poly[q.type, "term-synth"])],
    anonClassNameSuffix: String,
  ): qr.Term = {
    import qr.*

    val (tpe, termFn) = StructuralRefinement.forMode["term-synth"][Base](members, anonClassNameSuffix)
    val term = termFn(owner)
    Typed(term, TypeTree.of(using tpe.asType))
  }

  /** Returns a structurally typed term, i.e.
   *
   * ```scala
   * new Base with Selectable { <refinements> } : Base & Selectable { <refinements> }
   * ```
   *
   * Member definitions can take state and pass new state forward.
   */
  def typedTermStateful[Base](using baseType: Type[Base])[Q <: Quotes & Singleton](using q: Q)[S[_[_]]](
    owner : qr.Symbol,
    members: MemberDefsPoly[q.type, "term-synth", S],
    anonClassNameSuffix: String,
  ): (qr.Term, S[[x] =>> Unit]) = {
    import qr.*

    val (tpe, s, termFn) = StructuralRefinement.forModeStateful["term-synth"][Base](members, anonClassNameSuffix)
    val term = termFn(owner)
    (Typed(term, TypeTree.of(using tpe.asType)), s)
  }

  def forMode[M](using
    q: Quotes,
    mode: Mode[q.type, M],
  )[Base](using
    baseType: Type[Base],
  )(
    members: List[(String, MemberDef.Poly[q.type, M])],
    anonClassNameSuffix: String,
  ): (qr.TypeRepr, mode.OutEff[(owner: qr.Symbol) => qr.Term]) =
    val (tp, (), trm) = forModeStateful[M][Base](MemberDefsPoly.stateless(members), anonClassNameSuffix)
    (tp, trm)

  def forModeStateful[M](using
    q: Quotes,
    mode: Mode[q.type, M],
  )[Base](using
    baseType: Type[Base],
  )[S[_[_]]](
    members: MemberDefsPoly[q.type, M, S],
    anonClassNameSuffix: String,
  ): (qr.TypeRepr, S[[x] =>> Unit], mode.OutEff[(owner: qr.Symbol) => qr.Term]) = {
    import qr.*

    val baseTypeRepr = TypeRepr.of[Base]
    val selectableBase = AndType(baseTypeRepr, TypeRepr.of[reflect.Selectable])
    val parents =
      if (baseTypeRepr.typeSymbol.flags.is(Flags.Trait))
        List(TypeRepr.of[Object], baseTypeRepr, TypeRepr.of[reflect.Selectable])
      else
        List(baseTypeRepr, TypeRepr.of[reflect.Selectable])

    val (tpe, s) =
      refinementType(selectableBase) { b =>
        val ((_, b1), s) =
          members.foldLeft["type-synth"]((PreviousSiblings.empty["type-synth"](using q), b)) {
            [X] => (acc, name, defn) => {
              val (ctx, b) = acc
              val (m, x) = defn(ctx)
              val acc1 = m match
                case _: MemberDef.Type[q] =>
                  val (b1, ref) = b.addAbstractType(name)
                  (ctx.addType(name, ref), b1)
                case tm: MemberDef.Val[q, tref] =>
                  val (b1, ref) = b.addMember(name, tm.tpe)
                  (ctx.addTerm(name, ref), b1)
                case md: MemberDef.Method[q, tref] =>
                  val (b1, ref) = b.addMember(name, md.tpe)
                  (ctx.addTerm(name, ref), b1)
              (acc1, x)
            }
          }

        (b1.result, s)
      }

    (
      tpe,
      s,
      mode.isTermSynth.map { case TypeEq(Refl()) =>
        summon[M =:= "term-synth"]
        { (owner: Symbol) =>
          val clsSym =
            Symbol.newClass(
              owner,
              name = "$anon_" + anonClassNameSuffix,
              parents = parents,
              decls = selfSym => {
                val ((_, symsRev), _) =
                  members.foldLeft["type-synth"]((
                    PreviousSiblings.empty["type-synth"](using q),
                    List.empty[Symbol],
                  )) { [X] => (acc, name, defn) =>
                    val (ctx, syms) = acc
                    val (m, x) = defn(ctx)
                    val acc1 = m match
                      case td: MemberDef.Type[q] =>
                        val tp = td.body
                        val tpSym =
                          Symbol.newTypeAlias(
                            parent = selfSym,
                            name = name,
                            flags = Flags.EmptyFlags,
                            tpe = tp,
                            privateWithin = Symbol.noSymbol,
                          )
                        (ctx.addType(name, tpSym.typeRef), tpSym :: syms)
                      case tm: MemberDef.Val[q, term] =>
                        val sym =
                          Symbol.newVal(
                            parent = selfSym,
                            name = name,
                            tpe = tm.tpe,
                            flags = Flags.EmptyFlags,
                            privateWithin = Symbol.noSymbol,
                          )
                        (ctx.addTerm(name, sym.termRef), sym :: syms)
                      case md: MemberDef.Method[q, term] =>
                        val sym =
                          Symbol.newMethod(
                            parent = selfSym,
                            name = name,
                            tpe = md.tpe,
                            flags = Flags.EmptyFlags,
                            privateWithin = Symbol.noSymbol,
                          )
                        (ctx.addTerm(name, sym.termRef), sym :: syms)
                    (acc1, x)
                  }

                symsRev.reverse
              },
              selfType = None,
            )

          val definedTypeSymbols: List[Symbol] =
            clsSym.declaredTypes

          val definedTypeMap: Map[String, TypeRef] =
            definedTypeSymbols.map(sym => (sym.name, sym.typeRef)).toMap

          val definedValMap: Map[String, Term] =
            clsSym.declaredFields.map(sym => (sym.name, Ref.term(sym.termRef))).toMap

          // XXX: these are all the definitions, not just _previous_ ones
          val ctx = PreviousSiblings["term-synth"](
            definedTypeMap,
            mode.inTermProper.substituteContra(definedValMap),
          )

          val typeDefs =
            definedTypeSymbols.map(TypeDef(_))

          val valDefs =
            members.foldLeft["term-synth"](List.empty[Option[Definition]]) {
              [X] => (revAcc, name, defn) =>
                val (m, x) = defn(ctx)
                val revAcc1 =
                  m.acceptVisitor[Option[Definition]](
                    caseType = _ => None,
                    caseVal = (_, bodyF) => {
                      val body = mode.outEffId.at(bodyF)
                      val sym = clsSym.declaredField(name)
                      Some(ValDef(sym, Some(body(selfSym = sym))))
                    },
                    caseMethod = (_, bodyF) => {
                      val body = mode.outEffId.at(bodyF)
                      val sym =
                        clsSym.declaredMethod(name) match
                          case m :: Nil => m
                          case Nil => report.errorAndAbort(s"Bug: Method `$name` not found in declared methods")
                          case _ => report.errorAndAbort(s"Bug: Multiple methods named `$name` found in declared methods")
                      Some(DefDef(sym, argss => Some(body(sym, argss))))
                    }
                  )
                  :: revAcc
                (revAcc1, x)
            }._1.reverse.flatten

          val clsDef = ClassDef(
            clsSym,
            parents = parents.map(t => TypeTree.of(using t.asType)),
            body = typeDefs ++ valDefs,
          )

          val instance =
            Apply(Select(New(TypeIdent(clsSym)), clsSym.primaryConstructor), Nil)

          Block(
            List(clsDef),
            instance,
          )
        }
      },
    )
  }

  class PreviousSiblings[Q <: Quotes, M](using
    val q: Q,
    val mode: Mode[q.type, M],
  )(
    val types: Map[String, qr.TypeRef],
    val terms: Map[String, mode.InTerm],
  ) {
    def addType(name: String, value: qr.TypeRef): PreviousSiblings[Q, M] =
      new PreviousSiblings(
        types.updated(name, value),
        terms,
      )

    def addTerm(using m: Mode[q.type, M])(name: String, value: m.InTerm): PreviousSiblings[Q, M] =
      val terms1 = Mode.sameInTerm(mode, m).substituteCo(terms)
      new PreviousSiblings[Q, M](
        types,
        terms1.updated(name, value),
      )

    def termsProper(using M =:= "term-synth"): Map[String, qr.Term] =
      mode.inTermProper.substituteCo(terms)
  }

  object PreviousSiblings {
    def apply[M](using
      q: Quotes,
      mode: Mode[q.type, M],
    )(
      types: Map[String, qr.TypeRef],
      terms: Map[String, mode.InTerm],
    ): PreviousSiblings[q.type, M] =
      new PreviousSiblings[q.type, M](types, terms)

    def empty[M](using q: Quotes, m: Mode[q.type, M]): PreviousSiblings[q.type, M] =
      PreviousSiblings(Map.empty, Map.empty)
  }

  sealed trait MemberDef[Q <: Quotes, +F[_]] {
    def acceptVisitor[R](
      caseType: (q: Q) ?=> qr.TypeRepr => R,
      caseVal: (q: Q) ?=> (tpe: qr.TypeRepr, body: F[(selfSym: qr.Symbol) => qr.Term]) => R,
      caseMethod: (q: Q) ?=> (tpe: qr.MethodType, body: F[(selfSym: qr.Symbol, argss: List[List[qr.Tree]]) => qr.Term]) => R,
    ): R
  }

  object MemberDef {
    class Type[Q <: Quotes & Singleton](using val q: Q)(
      val body: qr.TypeRepr,
    ) extends MemberDef[Q, Nothing] {
      override def acceptVisitor[R](
        caseType: (q: Q) ?=> q.reflect.TypeRepr => R,
        caseVal: (q: Q) ?=> (tpe: q.reflect.TypeRepr, body: Nothing) => R,
        caseMethod: (q: Q) ?=> (tpe: q.reflect.MethodType, body: Nothing) => R,
      ): R =
        caseType(body)
    }

    class Val[Q <: Quotes & Singleton, F[_]](using val q: Q)(
      val tpe: qr.TypeRepr,
      val body: F[(selfSym: qr.Symbol) => qr.Term],
    ) extends MemberDef[Q, F] {
      override def acceptVisitor[R](
        caseType: (q: Q) ?=> q.reflect.TypeRepr => R,
        caseVal: (q: Q) ?=> (tpe: q.reflect.TypeRepr, body: F[(selfSym: q.reflect.Symbol) => q.reflect.Term]) => R,
        caseMethod: (q: Q) ?=> (tpe: q.reflect.MethodType, body: F[(selfSym: q.reflect.Symbol, argss: List[List[q.reflect.Tree]]) => q.reflect.Term]) => R,
      ): R =
        caseVal(tpe, body)
    }

    class Method[Q <: Quotes & Singleton, F[_]](using val q: Q)(
      val tpe: qr.MethodType,
      val body: F[(selfSym: qr.Symbol, argss: List[List[qr.Tree]]) => qr.Term],
    ) extends MemberDef[Q, F] {
      override def acceptVisitor[R](
        caseType: (q: Q) ?=> q.reflect.TypeRepr => R,
        caseVal: (q: Q) ?=> (tpe: q.reflect.TypeRepr, body: F[(selfSym: q.reflect.Symbol) => q.reflect.Term]) => R,
        caseMethod: (q: Q) ?=> (tpe: q.reflect.MethodType, body: F[(selfSym: q.reflect.Symbol, argss: List[List[q.reflect.Tree]]) => q.reflect.Term]) => R,
      ): R =
        caseMethod(tpe, body)
    }

    /** Helps with type-inference of the polymorphic function with given arguments. */
    opaque type Poly[Q <: Quotes, M]
      <: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> PreviousSiblings[Q, N] => MemberDef[Q, mode.OutEff]
      =  [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> PreviousSiblings[Q, N] => MemberDef[Q, mode.OutEff]

    def poly[Q <: Quotes, M](
      f: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> PreviousSiblings[Q, N] => MemberDef[Q, mode.OutEff],
    ): Poly[Q, M] =
      f

    /** Helps with type-inference of the polymorphic function with given arguments. */
    opaque type PolyS[Q <: Quotes, M, S[_[_]], T[_[_]]]
      <: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (acc: S[mode.OutEff], name: String, ctx: PreviousSiblings[Q, N]) => (T[mode.OutEff], MemberDef[Q, mode.OutEff])
      =  [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (acc: S[mode.OutEff], name: String, ctx: PreviousSiblings[Q, N]) => (T[mode.OutEff], MemberDef[Q, mode.OutEff])

    object PolyS {
      def apply[Q <: Quotes, M, S[_[_]], T[_[_]]](
        f: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (acc: S[mode.OutEff], name: String, ctx: PreviousSiblings[Q, N]) => (T[mode.OutEff], MemberDef[Q, mode.OutEff]),
      ): PolyS[Q, M, S, T] =
        f

      def writer[Q <: Quotes, M, W[_[_]]](
        f: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (name: String, ctx: PreviousSiblings[Q, N]) => (W[mode.OutEff], MemberDef[Q, mode.OutEff]),
      ): PolyS[Q, M, [f[_]] =>> Unit, W] =
        PolyS[Q, M, [f[_]] =>> Unit, W](
          [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (_: Unit, name: String, ctx: PreviousSiblings[Q, N]) => f[N](name, ctx)
        )

      def fromStateless[Q <: Quotes, M, S[_[_]]](m: MemberDef.Poly[Q, M]): MemberDef.PolyS[Q, M, S, [f[_]] =>> Unit] =
        PolyS([N] => (mode, sub) ?=> (_, _, ctx) => ((), m[N](ctx)))
    }
  }

  /**
    * @tparam Q (typically singleton) subtype of Quotes
    * @tparam M upper bound on mode
    * @tparam S accumulated output from member definitions, parameterized by the mode's `OutEff`.
    */
  sealed trait MemberDefsPoly[Q <: Quotes, M, S[_[_]]] {
    def next[T[_[_]]](
      nextName: String,
      nextDef: MemberDef.PolyS[Q, M, S, T],
    ): MemberDefsPoly[Q, M, T] =
      MemberDefsPoly.Snoc(this, nextName, nextDef)

    def foldLeft
      [N](using mode: Mode[Q, N], sub: N IsSubsumedBy M)
      [B](b: B)(f: [X] => (B, String, PreviousSiblings[Q, N] => (MemberDef[Q, mode.OutEff], X)) => (B, X))
    : (B, S[mode.OutEff])
  }

  object MemberDefsPoly {
    case class Empty[Q <: Quotes, M, S[_[_]]](
      initialState: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> S[mode.OutEff]
    ) extends MemberDefsPoly[Q, M, S] {
      override def foldLeft[N](using mode: Mode[Q, N], sub: N IsSubsumedBy M)[B](b: B)(f: [X] => (B, String, PreviousSiblings[Q, N] => (MemberDef[Q, mode.OutEff], X)) => (B, X)): (B, S[mode.OutEff]) =
        (b, initialState[N])
    }

    case class Snoc[Q <: Quotes, M, S[_[_]], T[_[_]]](
      init: MemberDefsPoly[Q, M, S],
      lastName: String,
      lastDef: MemberDef.PolyS[Q, M, S, T],
    ) extends MemberDefsPoly[Q, M, T] {
      override def foldLeft[N](using mode: Mode[Q, N], sub: N IsSubsumedBy M)[B](b: B)(f: [X] => (B, String, PreviousSiblings[Q, N] => (MemberDef[Q, mode.OutEff], X)) => (B, X)): (B, T[mode.OutEff]) =
        val (b1, s) = init.foldLeft[N](b)(f)
        f(b1, lastName, lastDef[N](s, lastName, _).swap)
    }

    def emptyUnit[Q <: Quotes, M]: MemberDefsPoly[Q, M, [f[_]] =>> Unit] =
      Empty[Q, M, [f[_]] =>> Unit]([N] => (mode, sub) ?=> ())

    def stateless[M](using
      q: Quotes,
      mode: Mode[q.type, M],
    )(
      members: List[(String, MemberDef.Poly[q.type, M])],
    ): MemberDefsPoly[q.type, M, [F[_]] =>> Unit] = {
      val init: MemberDefsPoly[q.type, M, [F[_]] =>> Unit] =
        Empty([N] => (mode: Mode[q.type, N], sub: N IsSubsumedBy M) ?=> ())
      members.foldLeft(init) { case (acc, (name, defn)) =>
        Snoc(
          acc,
          name,
          MemberDef.PolyS([N] => (mode: Mode[q.type, N], sub: N IsSubsumedBy M) ?=> (_, _, ctx) => ((), defn[N](ctx))),
        )
      }
    }
  }

  private def refinementType(using q: Quotes)(
    baseType: qr.TypeRepr,
  )[W](
    f: RefinementTypeBuilder[q.type] => (qr.TypeRepr, W)
  ): (qr.TypeRepr, W) = {
    var written: Option[W] = None
    val res = qr.RecursiveType { self =>
      assert(written.isEmpty, "Unexpected multiple invocations of `f` in `quotes.reflect.RecursiveType(f)")
      val (t, w) = f(RefinementTypeBuilder[q.type](q)(self, baseType))
      written = Some(w)
      t
    }
    assert(written.nonEmpty, "Unexpected non-invocation of `f` in `quotes.reflect.RecursiveType(f)")
    (res, written.get)
  }

  private class RefinementTypeBuilder[Q <: Quotes](
    val q: Q,
  )(
    self: q.reflect.RecursiveType,
    acc: q.reflect.TypeRepr
  ) {
    import q.reflect.*

    def addAbstractType(name: String): (RefinementTypeBuilder[Q], TypeRef) = {
      val acc1 = Refinement(acc, name, TypeBounds.empty)
      val ref = typeRefUnsafe(using q)(self.recThis, name)

      (RefinementTypeBuilder(q)(self, acc1), ref)
    }

    def addMember(name: String, tpe: TypeRepr): (RefinementTypeBuilder[Q], TermRef) = {
      val acc1 = Refinement(acc, name, tpe)
      val ref = TermRef(self.recThis, name)
      (RefinementTypeBuilder(q)(self, acc1), ref)
    }

    def result: TypeRepr =
      acc
  }

  def typeRefUnsafe(using q: Quotes)(prefix: qr.TypeRepr, name: String): qr.TypeRef = {
    // XXX: using compiler internals will backfire at some point
    import dotty.tools.dotc.core.{Names, Types}

    Types.TypeRef(
      prefix.asInstanceOf[Types.Type],
      Names.typeName(name),
    )(using
      q.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    ).asInstanceOf[qr.TypeRef]
  }

}
