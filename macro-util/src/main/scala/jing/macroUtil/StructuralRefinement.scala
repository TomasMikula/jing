package jing.macroUtil

import jing.macroUtil.Mode.IsSubsumedBy
import jing.macroUtil.Mode.IsSubsumedBy.given
import libretto.lambda.util.TypeEq
import libretto.lambda.util.TypeEq.Refl
import scala.annotation.experimental
import scala.quoted.*

@experimental("Relying on experimental macro features.")
object StructuralRefinement {

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
    path: String,
    debugPrint: String => Unit,
  ): qr.Term = {
    import qr.*

    val (tpe, termFn) =
      StructuralRefinement.forMode["term-synth"][Base](members, anonClassNameSuffix, path, debugPrint)
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
  def typedTermStateful[Base](using baseType: Type[Base])[Q <: Quotes & Singleton](using q: Q)[S](
    owner : qr.Symbol,
    members: MemberDefsPoly[q.type, "term-synth", S],
    anonClassNameSuffix: String,
    path: String,
    debugPrint: String => Unit,
  ): (qr.Term, S) = {
    import qr.*

    val (tpe, s, termFn) =
      StructuralRefinement.forModeStateful["term-synth"][Base](members, anonClassNameSuffix, path, debugPrint)
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
    path: String,
    debugPrint: String => Unit,
  ): (qr.TypeRepr, mode.OutEff[(owner: qr.Symbol) => qr.Term]) =
    val (tp, (), trm) =
      forModeStateful[M][Base](MemberDefsPoly.stateless(members), anonClassNameSuffix, path, debugPrint)
    (tp, trm)

  def forModeStateful[M](using
    q: Quotes,
    mode: Mode[q.type, M],
  )[Base](using
    baseType: Type[Base],
  )[S](
    members: MemberDefsPoly[q.type, M, S],
    anonClassNameSuffix: String,
    symbolPath: String,
    debugPrint: String => Unit,
  ): (qr.TypeRepr, S, mode.OutEff[(owner: qr.Symbol) => qr.Term]) = {
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
              val t0 = System.currentTimeMillis()
              val (ctx, b) = acc
              val (m, x) = defn(ctx)
              val (kind, acc1) = m match
                case td: MemberDef.Type[q] =>
                  val (b1, ref) =
                    if (td.isAbstract)
                      b.addTypeMember(name)
                    else
                      b.addTypeMember(name, Some(td.body))
                  "type" -> (ctx.addType(name, ref), b1)
                case tm: MemberDef.Val[q, tref] =>
                  val (b1, ref) = b.addMember(name, tm.tpe)
                  "val " -> (ctx.addTerm(name, ref), b1)
                case md: MemberDef.Method[q, tref] =>
                  val (b1, ref) = b.addMember(name, md.tpe)
                  "def " -> (ctx.addTerm(name, ref), b1)
              val t1 = System.currentTimeMillis()
              debugPrint(s"Synthesized type       of $kind member $symbolPath.$name (${t1 - t0}ms)")
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
                    val t0 = System.currentTimeMillis()
                    val (ctx, syms) = acc
                    val (m, x) = defn(ctx)
                    val (kind, acc1) = m match
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
                        "type" -> (ctx.addType(name, tpSym.typeRef), tpSym :: syms)
                      case tm: MemberDef.Val[q, term] =>
                        val sym =
                          Symbol.newVal(
                            parent = selfSym,
                            name = name,
                            tpe = tm.tpe,
                            flags = Flags.EmptyFlags,
                            privateWithin = Symbol.noSymbol,
                          )
                        "val " -> (ctx.addTerm(name, sym.termRef), sym :: syms)
                      case md: MemberDef.Method[q, term] =>
                        val sym =
                          Symbol.newMethod(
                            parent = selfSym,
                            name = name,
                            tpe = md.tpe,
                            flags = Flags.EmptyFlags,
                            privateWithin = Symbol.noSymbol,
                          )
                        "def " -> (ctx.addTerm(name, sym.termRef), sym :: syms)
                    val t1 = System.currentTimeMillis()
                    debugPrint(s"Synthesized symbol     of $kind member $symbolPath.$name (${t1 - t0}ms)")
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
                val t0 = System.currentTimeMillis()
                val (m, x) = defn(ctx)
                val (kind, optDef) =
                  m.acceptVisitor[(String, Option[Definition])](
                    caseType = _ =>
                      "type" -> None,
                    caseVal = (_, bodyF) => {
                      val body = mode.outEffId.at(bodyF)
                      val sym = clsSym.declaredField(name)
                      "val " -> Some(ValDef(sym, Some(body(selfSym = sym))))
                    },
                    caseMethod = (_, bodyF) => {
                      val body = mode.outEffId.at(bodyF)
                      val sym =
                        clsSym.declaredMethod(name) match
                          case m :: Nil => m
                          case Nil => report.errorAndAbort(s"Bug: Method `$name` not found in declared methods")
                          case _ => report.errorAndAbort(s"Bug: Multiple methods named `$name` found in declared methods")
                      "def " -> Some(DefDef(sym, argss => Some(body(sym, argss))))
                    }
                  )
                val revAcc1 = optDef :: revAcc
                val t1 = System.currentTimeMillis()
                println(s"Synthesized definition of $kind member $symbolPath.$name (${t1 - t0}ms)")
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
      val isAbstract: Boolean,
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
    opaque type PolyS[Q <: Quotes, M, S, T]
      <: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (acc: S, ctx: PreviousSiblings[Q, N]) => (T, MemberDef[Q, mode.OutEff])
      =  [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (acc: S, ctx: PreviousSiblings[Q, N]) => (T, MemberDef[Q, mode.OutEff])

    object PolyS {
      def apply[Q <: Quotes, M, S, T](
        f: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (acc: S, ctx: PreviousSiblings[Q, N]) => (T, MemberDef[Q, mode.OutEff]),
      ): PolyS[Q, M, S, T] =
        f

      def writer[Q <: Quotes, M, W](
        f: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (ctx: PreviousSiblings[Q, N]) => (W, MemberDef[Q, mode.OutEff]),
      ): PolyS[Q, M, Unit, W] =
        PolyS[Q, M, Unit, W](
          [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (_: Unit, ctx: PreviousSiblings[Q, N]) => f[N](ctx)
        )

      def reader[Q <: Quotes, M, R](
        f: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> (env: R, ctx: PreviousSiblings[Q, N]) => MemberDef[Q, mode.OutEff],
      ): PolyS[Q, M, R, Unit] =
        PolyS[Q, M, R, Unit](
          [N] => (mode, sub) ?=> (env, ctx) => ((), f[N](env, ctx))
        )

      def fromStateless[Q <: Quotes, M, S](m: MemberDef.Poly[Q, M]): MemberDef.PolyS[Q, M, S, Unit] =
        PolyS([N] => (mode, sub) ?=> (_, ctx) => ((), m[N](ctx)))
    }
  }

  /**
    * @tparam Q (typically singleton) subtype of Quotes
    * @tparam M upper bound on mode
    * @tparam S accumulated output from member definitions
    */
  sealed trait MemberDefsPoly[Q <: Quotes, M, S] {
    def next[T](
      nextName: String,
      nextDef: MemberDef.PolyS[Q, M, S, T],
    ): MemberDefsPoly[Q, M, T] =
      MemberDefsPoly.Snoc(this, nextName, nextDef)

    def foldLeft
      [N](using mode: Mode[Q, N], sub: N IsSubsumedBy M)
      [B](b: B)(f: [X] => (B, String, PreviousSiblings[Q, N] => (MemberDef[Q, mode.OutEff], X)) => (B, X))
    : (B, S)
  }

  object MemberDefsPoly {
    case class Empty[Q <: Quotes, M, S](
      initialState: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> S,
    ) extends MemberDefsPoly[Q, M, S] {
      override def foldLeft[N](using
        mode: Mode[Q, N],
        sub: N IsSubsumedBy M,
      )[B](
        b: B,
      )(
        f: [X] => (B, String, PreviousSiblings[Q, N] => (MemberDef[Q, mode.OutEff], X)) => (B, X),
      ): (B, S) =
        (b, initialState[N])
    }

    case class Snoc[Q <: Quotes, M, S, T](
      init: MemberDefsPoly[Q, M, S],
      lastName: String,
      lastDef: MemberDef.PolyS[Q, M, S, T],
    ) extends MemberDefsPoly[Q, M, T] {
      override def foldLeft[N](using
        mode: Mode[Q, N],
        sub: N IsSubsumedBy M,
      )[B](
        b: B,
      )(
        f: [X] => (B, String, PreviousSiblings[Q, N] => (MemberDef[Q, mode.OutEff], X)) => (B, X),
      ): (B, T) =
        val (b1, s) = init.foldLeft[N](b)(f)
        f(b1, lastName, lastDef[N](s, _).swap)
    }

    def emptyUnit[Q <: Quotes, M]: MemberDefsPoly[Q, M, Unit] =
      Empty[Q, M, Unit]([N] => (mode, sub) ?=> ())

    def stateless[M](using
      q: Quotes,
      mode: Mode[q.type, M],
    )(
      members: List[(String, MemberDef.Poly[q.type, M])],
    ): MemberDefsPoly[q.type, M, Unit] = {
      val init: MemberDefsPoly[q.type, M, Unit] =
        Empty([N] => (mode: Mode[q.type, N], sub: N IsSubsumedBy M) ?=> ())
      members.foldLeft(init) { case (acc, (name, defn)) =>
        Snoc(
          acc,
          name,
          MemberDef.PolyS([N] => (mode: Mode[q.type, N], sub: N IsSubsumedBy M) ?=> (_, ctx) => ((), defn[N](ctx))),
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

    def addTypeMember(name: String, tpe: Option[TypeRepr] = None): (RefinementTypeBuilder[Q], TypeRef) = {
      val typeBounds = tpe match
        case Some(tpe) => TypeBounds(tpe, tpe)
        case None => TypeBounds.empty
      val acc1 = Refinement(acc, name, typeBounds)
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
