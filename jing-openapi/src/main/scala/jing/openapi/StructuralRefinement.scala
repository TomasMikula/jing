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

  def forMode[M](using
    q: Quotes,
    mode: Mode[q.type, M],
  )[Base](using
    baseType: Type[Base],
  )(
    members : List[(String, MemberDef.Poly[q.type, M])],
    anonClassNameSuffix: String,
  ): (qr.TypeRepr, mode.OutEff[(owner: qr.Symbol) => qr.Term]) = {
    import qr.*

    val baseTypeRepr = TypeRepr.of[Base]
    val selectableBase = AndType(baseTypeRepr, TypeRepr.of[reflect.Selectable])
    val parents =
      if (baseTypeRepr.typeSymbol.flags.is(Flags.Trait))
        List(TypeRepr.of[Object], baseTypeRepr, TypeRepr.of[reflect.Selectable])
      else
        List(baseTypeRepr, TypeRepr.of[reflect.Selectable])

    (
      refinementType(selectableBase) { b =>
        val (_, b1) =
          members.foldLeft((PreviousSiblings.empty["type-synth"](using q), b)) {
            case ((ctx, b), (name, defn)) =>
              defn["type-synth"](ctx) match
                case _: MemberDef.Type[q] =>
                  val (b1, ref) = b.addAbstractType(name)
                  (ctx.addType(name, ref), b1)
                case tm: MemberDef.Val[q, tref] =>
                  val (b1, ref) = b.addMember(name, tm.tpe)
                  (ctx.addTerm(name, ref), b1)
                case md: MemberDef.Method[q, tref] =>
                  val (b1, ref) = b.addMember(name, md.tpe)
                  (ctx.addTerm(name, ref), b1)
          }

        b1.result
      },

      mode.isTermSynth.map { case TypeEq(Refl()) =>
        summon[M =:= "term-synth"]
        { (owner: Symbol) =>
          val clsSym =
            Symbol.newClass(
              owner,
              name = "$anon_" + anonClassNameSuffix,
              parents = parents,
              decls = selfSym => {
                val (_, symsRev) =
                  members.foldLeft((
                    PreviousSiblings.empty["type-synth"](using q),
                    List.empty[Symbol],
                  )) { case ((ctx, acc), (name, defn)) =>
                    defn["type-synth"](ctx) match
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
                        (ctx.addType(name, tpSym.typeRef), tpSym :: acc)
                      case tm: MemberDef.Val[q, term] =>
                        val sym =
                          Symbol.newVal(
                            parent = selfSym,
                            name = name,
                            tpe = tm.tpe,
                            flags = Flags.EmptyFlags,
                            privateWithin = Symbol.noSymbol,
                          )
                        (ctx.addTerm(name, sym.termRef), sym :: acc)
                      case md: MemberDef.Method[q, term] =>
                        val sym =
                          Symbol.newMethod(
                            parent = selfSym,
                            name = name,
                            tpe = md.tpe,
                            flags = Flags.EmptyFlags,
                            privateWithin = Symbol.noSymbol,
                          )
                        (ctx.addTerm(name, sym.termRef), sym :: acc)
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
            members.flatMap { case (name, defn) =>
              defn["term-synth"](ctx)
                .acceptVisitor[Option[Definition]](
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
            }

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

    def poly[Q <: Quotes, M](using q: Q)(
      f: [N] => (mode: Mode[Q, N], sub: N IsSubsumedBy M) ?=> PreviousSiblings[Q, N] => MemberDef[Q, mode.OutEff],
    ): Poly[Q, M] =
      f
  }

  private def refinementType(using q: Quotes)(
    baseType: qr.TypeRepr,
  )(
    f: RefinementTypeBuilder[q.type] => qr.TypeRepr
  ): qr.TypeRepr =
    qr.RecursiveType { self =>
      f(RefinementTypeBuilder[q.type](q)(self, baseType))
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
