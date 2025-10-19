package jing.openapi.model

import jing.openapi.model.IsPropertyOf.{IsOptionalPropertyOf, IsRequiredPropertyOf}
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq.Refl
import libretto.lambda.util.{Applicative, BiInjective, Exists, SingletonType, TypeEq, TypeEqK}

import scala.NamedTuple.{AnyNamedTuple, DropNames, NamedTuple, Names}

sealed trait ObjectMotif[Req[_], Opt[_], Props] {
  import ObjectMotif.*

  def get[K](using i: IsPropertyOf[K, Props]): i.ReqOrOpt[Req, Opt][i.Type]

  def getOpt(k: String): Option[Exists[[A] =>> Req[A] | Opt[A]]]

  def getOptFull(k: String): Option[Exists[[A] =>>
    Either[
      (IsRequiredPropertyOf.Aux[k.type, Props, A], Req[A]),
      (IsOptionalPropertyOf.Aux[k.type, Props, A], Opt[A]),
    ]
  ]]

  def traverse[M[_], G[_], H[_]](
    g: [A] => Req[A] => M[G[A]],
    h: [A] => Opt[A] => M[H[A]],
  )(using
    Applicative[M],
  ): M[ObjectMotif[G, H, Props]]

  def translate[G[_], H[_]](
    g: [A] => Req[A] => G[A],
    h: [A] => Opt[A] => H[A],
  ): ObjectMotif[G, H, Props] =
    traverse[[x] =>> x, G, H](g, h)

  def wipeTranslate[G[_], H[_]](
    g: [A] => Req[A] => Exists[G],
    h: [A] => Opt[A] => Exists[H],
  ): Exists[[X] =>> ObjectMotif[G, H, X]] =
    wipeTranslateA[[x] =>> x, G, H](g, h)

  def wipeTranslateA[M[_], G[_], H[_]](
    g: [A] => Req[A] => M[Exists[G]],
    h: [A] => Opt[A] => M[Exists[H]],
  )(using
    Applicative[M],
  ): M[Exists[[X] =>> ObjectMotif[G, H, X]]]

  def zipWithNamedTuple[G[_], H[_], I[_], J[_]](
    t: NamedTuple[PropNamesTuple[Props], PropTypesTupleF[G, H, Props]],
  )(
    fReq: [A] => (Req[A], G[A]) => I[A],
    fOpt: [A] => (Opt[A], H[A]) => J[A],
  ): ObjectMotif[I, J, Props] =
    zipWithNamedTupleAcc[G, H, EmptyTuple, EmptyTuple, I, J](t)(fReq, fOpt)._1

  protected def zipWithNamedTupleAcc[G[_], H[_], NAcc <: Tuple, TAcc <: Tuple, I[_], J[_]](
    t: NamedTuple[PropNamesTupleAcc[Props, NAcc], PropTypesTupleFAcc[G, H, Props, TAcc]],
  )(
    fReq: [A] => (Req[A], G[A]) => I[A],
    fOpt: [A] => (Opt[A], H[A]) => J[A],
  ): (ObjectMotif[I, J, Props], TAcc)

  def toNamedTuple[G[_], H[_]](
    g: [A] => Req[A] => G[A],
    h: [A] => Opt[A] => H[A],
  ): NamedTuple[PropNamesTuple[Props], PropTypesTupleF[G, H, Props]] =
    toTuple[G, H](g, h)

  def toTuple[G[_], H[_]](
    g: [A] => Req[A] => G[A],
    h: [A] => Opt[A] => H[A],
  ): PropTypesTupleF[G, H, Props] =
    toTupleAcc[G, H, EmptyTuple](g, h, EmptyTuple)

  protected def toTupleAcc[G[_], H[_], TAcc <: Tuple](
    g: [A] => Req[A] => G[A],
    h: [A] => Opt[A] => H[A],
    acc: TAcc,
  ): PropTypesTupleFAcc[G, H, Props, TAcc]

}

object ObjectMotif {
  case class Empty[Req[_], Opt[_]]() extends ObjectMotif[Req, Opt, Void] {

    override def get[K](using i: K IsPropertyOf Void): i.ReqOrOpt[Req, Opt][i.Type] =
      i.propertiesNotVoid

    override def getOpt(k: String): Option[Exists[[A] =>> Req[A] | Opt[A]]] =
      None

    override def getOptFull(k: String): Option[Exists[[A] =>>
      Either[
        (IsRequiredPropertyOf.Aux[k.type, Void, A], Req[A]),
        (IsOptionalPropertyOf.Aux[k.type, Void, A], Opt[A]),
      ]
    ]] =
      None

    override def traverse[M[_], G[_], H[_]](
      g: [A] => Req[A] => M[G[A]],
      h: [A] => Opt[A] => M[H[A]],
    )(using
      M: Applicative[M],
    ): M[ObjectMotif[G, H, Void]] =
      M.pure(Empty())

    override def wipeTranslateA[M[_], G[_], H[_]](
      g: [A] => Req[A] => M[Exists[G]],
      h: [A] => Opt[A] => M[Exists[H]],
    )(using
      M: Applicative[M],
    ): M[Exists[[X] =>> ObjectMotif[G, H, X]]] =
      M.pure(Indeed(Empty()))

    override protected def zipWithNamedTupleAcc[G[_], H[_], NAcc <: Tuple, TAcc <: Tuple, I[_], J[_]](
      t: NamedTuple[PropNamesTupleAcc[Void, NAcc], PropTypesTupleFAcc[G, H, Void, TAcc]],
    )(
      fReq: [A] => (Req[A], G[A]) => I[A],
      fOpt: [A] => (Opt[A], H[A]) => J[A],
    ): (ObjectMotif[I, J, Void], TAcc) =
      (Empty(), t: TAcc)

    override protected def toTupleAcc[G[_], H[_], TAcc <: Tuple](
      g: [A] => Req[A] => G[A],
      h: [A] => Opt[A] => H[A],
      acc: TAcc,
    ): PropTypesTupleFAcc[G, H, Void, TAcc] =
      acc

  }

  sealed trait NonEmpty[Req[_], Opt[_], Ps] extends ObjectMotif[Req, Opt, Ps]

  case class Snoc[Req[_], Opt[_], Init, PropName <: String, PropType](
    init: ObjectMotif[Req, Opt, Init],
    pname: SingletonType[PropName],
    pval: Req[PropType],
  ) extends ObjectMotif.NonEmpty[Req, Opt, Init || PropName :: PropType] {

    override def get[K](using i: K IsPropertyOf Init || PropName :: PropType): i.ReqOrOpt[Req, Opt][i.Type] =
      i.switch[i.ReqOrOpt[Req, Opt][i.Type]](
        caseLastProp =
          [init] => (
            ev1: (Init || PropName :: PropType) =:= (init || K :: i.Type),
            ev2: [F[_], G[_]] => DummyImplicit ?=> TypeEqK[i.ReqOrOpt[F, G], F],
          ) => {
            ev1 match
              case BiInjective[||](_, BiInjective[::](_, TypeEq(Refl()))) =>
                ev2[Req, Opt].flip.at[i.Type](pval)
          },
        caseOptLastProp =
          [init] => (
            ev1: (Init || PropName :: PropType) =:= (init || K :? i.Type),
            ev2: [F[_], G[_]] => DummyImplicit ?=> TypeEqK[i.ReqOrOpt[F, G], G],
          ) => {
            ev1 match
              case BiInjective[||](_, ev) => :?.isNot_::[K, i.Type, PropName, PropType](using ev.flip)
          },
        caseInitProp =
          [init, last] => (
            ev1: (Init || PropName :: PropType) =:= (init || last),
            j:   IsPropertyOf.Aux[K, init, i.Type, i.ReqOrOpt],
          ) => {
            val ev: j.ReqOrOpt[Req, Opt][j.Type] =:= i.ReqOrOpt[Req, Opt][i.Type] =
              summon[TypeEqK[j.ReqOrOpt[Req, Opt], i.ReqOrOpt[Req, Opt]]]
                .atH[j.Type, i.Type]
            ev1 match
              case BiInjective[||](TypeEq(Refl()), _) =>
                ev(init.get[K](using j))
          },
      )

    override def getOpt(k: String): Option[Exists[[A] =>> Req[A] | Opt[A]]] =
      if (k == pname.value)
        Some(Exists(pval))
      else
        init.getOpt(k)

    override def getOptFull(k: String): Option[Exists[[A] =>>
      Either[
        (IsRequiredPropertyOf.Aux[k.type, Init || PropName :: PropType, A], Req[A]),
        (IsOptionalPropertyOf.Aux[k.type, Init || PropName :: PropType, A], Opt[A]),
      ]
    ]] =
      if (k == pname.value)
        val ev: k.type =:= PropName = summon[k.type =:= k.type].asInstanceOf
        val i = summon[IsRequiredPropertyOf.Aux[PropName, Init || PropName :: PropType, PropType]]
        val j = ev.substituteContra[IsRequiredPropertyOf.Aux[_, Init || PropName :: PropType, PropType]](i)
        Some(Exists(Left((j, pval))))
      else
        init
          .getOptFull(k)
          .map {
            case Indeed(Left((i, v))) => Indeed(Left((i.inInit, v)))
            case Indeed(Right((i, v))) => Indeed(Right((i.inInit, v)))
          }

    override def traverse[M[_], G[_], H[_]](
      g: [A] => Req[A] => M[G[A]],
      h: [A] => Opt[A] => M[H[A]],
    )(using
      M: Applicative[M],
    ): M[ObjectMotif[G, H, Init || PropName :: PropType]] =
      M.map2(
        init.traverse(g, h),
        g(pval)
      ): (init, pval) =>
        Snoc(init, pname, pval)

    override def wipeTranslate[G[_], H[_]](
      g: [A] => Req[A] => Exists[G],
      h: [A] => Opt[A] => Exists[H],
    ): Exists[[X] =>> ObjectMotif[G, H, X]] =
      Indeed(Snoc(init.wipeTranslate(g, h).value, pname, g(pval).value))

    override def wipeTranslateA[M[_], G[_], H[_]](
      g: [A] => Req[A] => M[Exists[G]],
      h: [A] => Opt[A] => M[Exists[H]],
    )(using
      M: Applicative[M],
    ): M[Exists[[X] =>> ObjectMotif[G, H, X]]] =
      M.map2(
        init.wipeTranslateA(g, h),
        g(pval),
      ): (init, pval) =>
        Indeed(Snoc(init.value, pname, pval.value))

    override protected def zipWithNamedTupleAcc[G[_], H[_], NAcc <: Tuple, TAcc <: Tuple, I[_], J[_]](
      t: NamedTuple[PropNamesTupleAcc[Init || PropName :: PropType, NAcc], PropTypesTupleFAcc[G, H, Init || PropName :: PropType, TAcc]],
    )(
      fReq: [A] => (Req[A], G[A]) => I[A],
      fOpt: [A] => (Opt[A], H[A]) => J[A],
    ): (ObjectMotif[I, J, Init || PropName :: PropType], TAcc) = {
      type NAcc1 =   PropName  *: NAcc
      type TAcc1 = G[PropType] *: TAcc
      val (initH, acc1) =
        init.zipWithNamedTupleAcc[G, H, NAcc1, TAcc1, I, J](
          t: NamedTuple[PropNamesTupleAcc[Init, NAcc1], PropTypesTupleFAcc[G, H, Init, TAcc1]]
        )(fReq, fOpt)
      val g: G[PropType] =
        acc1.head
      val i: I[PropType] =
        fReq(pval, g)
      val acc: TAcc =
        acc1.tail
      (Snoc(initH, pname, i), acc)
    }

    override protected def toTupleAcc[G[_], H[_], TAcc <: Tuple](
      g: [A] => Req[A] => G[A],
      h: [A] => Opt[A] => H[A],
      acc: TAcc,
    ): PropTypesTupleFAcc[G, H, Init || PropName :: PropType, TAcc] =
      init.toTupleAcc[G, H, G[PropType] *: TAcc](g, h, g(pval) *: acc)

  }

  case class SnocOpt[Req[_], Opt[_], Init, PropName <: String, PropType](
    init: ObjectMotif[Req, Opt, Init],
    pname: SingletonType[PropName],
    pval: Opt[PropType],
  ) extends ObjectMotif.NonEmpty[Req, Opt, Init || PropName :? PropType] {

    override def get[K](using i: K IsPropertyOf Init || PropName :? PropType): i.ReqOrOpt[Req, Opt][i.Type] =
      i.switch[i.ReqOrOpt[Req, Opt][i.Type]](
        caseLastProp =
          [init] => (
            ev1: (Init || PropName :? PropType) =:= (init || K :: i.Type),
            ev2: [F[_], G[_]] => DummyImplicit ?=> TypeEqK[i.ReqOrOpt[F, G], F],
          ) => {
            ev1 match
              case BiInjective[||](_, ev) =>
                :?.isNot_::(using ev)
          },
        caseOptLastProp =
          [init] => (
            ev1: (Init || PropName :? PropType) =:= (init || K :? i.Type),
            ev2: [F[_], G[_]] => DummyImplicit ?=> TypeEqK[i.ReqOrOpt[F, G], G],
          ) => {
            ev1 match
              case BiInjective[||](_, BiInjective[:?](_, TypeEq(Refl()))) =>
                ev2[Req, Opt].flip.at[i.Type](pval)
          },
        caseInitProp =
          [init, last] => (
            ev1: (Init || PropName :? PropType) =:= (init || last),
            j:   IsPropertyOf.Aux[K, init, i.Type, i.ReqOrOpt],
          ) => {
            val ev =
              summon[TypeEqK[j.ReqOrOpt[Req, Opt], i.ReqOrOpt[Req, Opt]]]
                .atH[j.Type, i.Type]
            ev1 match
              case BiInjective[||](TypeEq(Refl()), _) =>
                ev(init.get[K](using j))
          },
      )

    override def getOpt(k: String): Option[Exists[[A] =>> Req[A] | Opt[A]]] =
      if (k == pname.value)
        Some(Exists(pval))
      else
        init.getOpt(k)

    override def getOptFull(k: String): Option[Exists[[A] =>>
      Either[
        (IsRequiredPropertyOf.Aux[k.type, Init || PropName :? PropType, A], Req[A]),
        (IsOptionalPropertyOf.Aux[k.type, Init || PropName :? PropType, A], Opt[A]),
      ]
    ]] =
      if (k == pname.value)
        val ev: k.type =:= PropName = summon[k.type =:= k.type].asInstanceOf
        val i = summon[IsOptionalPropertyOf.Aux[PropName, Init || PropName :? PropType, PropType]]
        val j = ev.substituteContra[IsOptionalPropertyOf.Aux[_, Init || PropName :? PropType, PropType]](i)
        Some(Exists(Right((j, pval))))
      else
        init
          .getOptFull(k)
          .map {
            case Indeed(Left((i, v))) => Indeed(Left((i.inInit, v)))
            case Indeed(Right((i, v))) => Indeed(Right((i.inInit, v)))
          }

    override def traverse[M[_], G[_], H[_]](
      g: [A] => Req[A] => M[G[A]],
      h: [A] => Opt[A] => M[H[A]],
    )(using
      M: Applicative[M],
    ): M[ObjectMotif[G, H, Init || PropName :? PropType]] =
      M.map2(
        init.traverse(g, h),
        h(pval),
      ): (init, pval) =>
        SnocOpt(init, pname, pval)

    override def wipeTranslateA[M[_], G[_], H[_]](
      g: [A] => Req[A] => M[Exists[G]],
      h: [A] => Opt[A] => M[Exists[H]],
    )(using
      M: Applicative[M],
    ): M[Exists[[X] =>> ObjectMotif[G, H, X]]] =
      M.map2(
        init.wipeTranslateA(g, h),
        h(pval),
      ): (init, pval) =>
        Indeed(SnocOpt(init.value, pname, pval.value))

    override protected def zipWithNamedTupleAcc[G[_], H[_], NAcc <: Tuple, TAcc <: Tuple, I[_], J[_]](
      t: NamedTuple[PropNamesTupleAcc[Init || PropName :? PropType, NAcc], PropTypesTupleFAcc[G, H, Init || PropName :? PropType, TAcc]],
    )(
      fReq: [A] => (Req[A], G[A]) => I[A],
      fOpt: [A] => (Opt[A], H[A]) => J[A],
    ): (ObjectMotif[I, J, Init || PropName :? PropType], TAcc) = {
      type NAcc1 =   PropName  *: NAcc
      type TAcc1 = H[PropType] *: TAcc
      val (initH, acc1) =
        init.zipWithNamedTupleAcc[G, H, NAcc1, TAcc1, I, J](
          t: NamedTuple[PropNamesTupleAcc[Init, NAcc1], PropTypesTupleFAcc[G, H, Init, TAcc1]]
        )(fReq, fOpt)
      val h: H[PropType] =
        acc1.head
      val j: J[PropType] =
        fOpt(pval, h)
      val acc: TAcc =
        acc1.tail
      (SnocOpt(initH, pname, j), acc)
    }

    override protected def toTupleAcc[G[_], H[_], TAcc <: Tuple](
      g: [A] => Req[A] => G[A],
      h: [A] => Opt[A] => H[A],
      acc: TAcc,
    ): PropTypesTupleFAcc[G, H, Init || PropName :? PropType, TAcc] =
      init.toTupleAcc[G, H, H[PropType] *: TAcc](g, h, h(pval) *: acc)

  }
}
