package jing.openapi.model

import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq.Refl
import libretto.lambda.util.{Applicative, BiInjective, Exists, SingletonType, TypeEq, TypeEqK}

import scala.NamedTuple.{AnyNamedTuple, DropNames, NamedTuple, Names}

sealed trait ObjectMotif[F[_ <: ObjectMotif.Mod, _], Props] {
  import ObjectMotif.*

  def get[K](using i: IsPropertyOf[K, Props]): F[i.Mod, i.Type]

  def traverse[M[_], G[_ <: Mod, _]](
    f: [A] => F[Mod.Required.type, A] => M[G[Mod.Required.type, A]],
    g: [A] => F[Mod.Optional.type, A] => M[G[Mod.Optional.type, A]],
  )(using
    Applicative[M],
  ): M[ObjectMotif[G, Props]]

  def translate[G[_ <: Mod, _]](
    f: [M <: Mod, A] => F[M, A] => G[M, A],
  ): ObjectMotif[G, Props]

  def wipeTranslate[H[_ <: Mod, _]](
    f: [M <: Mod, A] => F[M, A] => Exists[H[M, _]],
  ): Exists[[X] =>> ObjectMotif[H, X]]

  def wipeTranslateA[G[_], H[_ <: Mod, _]](
    f: [M <: Mod, A] => F[M, A] => G[Exists[H[M, _]]],
  )(using
    Applicative[G],
  ): G[Exists[[X] =>> ObjectMotif[H, X]]]

  def zipWithNamedTuple[G[_], H[_ <: Mod, _]](t: PropsToNamedTuple[G, Props])(
    fReq: [A] => (F[Mod.Required.type, A], G[A]) => H[Mod.Required.type, A],
    fOpt: [A] => (F[Mod.Optional.type, A], G[A] | None.type) => H[Mod.Optional.type, A],
  ): ObjectMotif[H, Props] =
    zipWithNamedTupleAcc[G, NamedTuple.Empty, H](t)(fReq, fOpt)._1

  protected def zipWithNamedTupleAcc[G[_], Acc <: AnyNamedTuple, H[_ <: Mod, _]](
    t: PropsToNamedTupleAcc[G, Props, Acc],
  )(
    fReq: [A] => (F[Mod.Required.type, A], G[A]) => H[Mod.Required.type, A],
    fOpt: [A] => (F[Mod.Optional.type, A], G[A] | None.type) => H[Mod.Optional.type, A],
  ): (ObjectMotif[H, Props], Acc)

  def zipWithNamedTuple2[G[_], H[_ <: Mod, _]](t: NamedTuple[PropNamesTuple[Props], PropTypesTuple[G, Props]])(
    fReq: [A] => (F[Mod.Required.type, A], G[A]) => H[Mod.Required.type, A],
    fOpt: [A] => (F[Mod.Optional.type, A], G[A] | None.type) => H[Mod.Optional.type, A],
  ): ObjectMotif[H, Props] =
    zipWithNamedTuple2Acc[G, EmptyTuple, EmptyTuple, H](t)(fReq, fOpt)._1

  def zipWithNamedTuple2Acc[G[_], NAcc <: Tuple, TAcc <: Tuple, H[_ <: Mod, _]](
    t: NamedTuple[PropNamesTupleAcc[Props, NAcc], PropTypesTupleAcc[G, Props, TAcc]],
  )(
    fReq: [A] => (F[Mod.Required.type, A], G[A]) => H[Mod.Required.type, A],
    fOpt: [A] => (F[Mod.Optional.type, A], G[A] | None.type) => H[Mod.Optional.type, A],
  ): (ObjectMotif[H, Props], TAcc)

}

object ObjectMotif {
  enum Mod:
    case Required
    case Optional

  import Mod.*

  case class Empty[F[_ <: Mod, _]]() extends ObjectMotif[F, Void] {

    override def get[K](using i: K IsPropertyOf Void): F[i.Mod, i.Type] =
      i.propertiesNotVoid

    override def traverse[M[_], G[_ <: Mod,_]](
      f: [A] => F[Mod.Required.type, A] => M[G[Mod.Required.type, A]],
      g: [A] => F[Mod.Optional.type, A] => M[G[Mod.Optional.type, A]],
    )(using
      M: Applicative[M],
    ): M[ObjectMotif[G, Void]] =
      M.pure(Empty())

    override def translate[G[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[M, A],
    ): ObjectMotif[G, Void] =
      Empty()

    override def wipeTranslate[H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => Exists[[X] =>> H[M, X]],
    ): Exists[[X] =>> ObjectMotif[H, X]] =
      Indeed(Empty())

    override def wipeTranslateA[G[_], H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[Exists[[X] =>> H[M, X]]],
    )(using
      G: Applicative[G],
    ): G[Exists[[X] =>> ObjectMotif[H, X]]] =
      G.pure(Indeed(Empty()))

    override protected def zipWithNamedTupleAcc[G[_], Acc <: AnyNamedTuple, H[_ <: Mod,_]](
      t: PropsToNamedTupleAcc[G, Void, Acc],
    )(
      fReq: [A] => (F[Required.type, A], G[A]) => H[Required.type, A],
      fOpt: [A] => (F[Optional.type, A], G[A] | None.type) => H[Optional.type, A],
    ): (ObjectMotif[H, Void], Acc) =
      (Empty(), t: Acc)

    override def zipWithNamedTuple2Acc[G[_], NAcc <: Tuple, TAcc <: Tuple, H[_ <: Mod,_]](
      t: NamedTuple[PropNamesTupleAcc[Void, NAcc], PropTypesTupleAcc[G, Void, TAcc]],
    )(
      fReq: [A] => (F[Required.type, A], G[A]) => H[Required.type, A],
      fOpt: [A] => (F[Optional.type, A], G[A] | None.type) => H[Optional.type, A],
    ): (ObjectMotif[H, Void], TAcc) =
      (Empty(), t: TAcc)

  }

  sealed trait NonEmpty[F[_ <: Mod, _], Ps] extends ObjectMotif[F, Ps]

  case class Snoc[F[_ <: Mod, _], Init, PropName <: String, PropType](
    init: ObjectMotif[F, Init],
    pname: SingletonType[PropName],
    pval: F[Mod.Required.type, PropType],
  ) extends ObjectMotif.NonEmpty[F, Init || PropName :: PropType] {

    override def get[K](using i: IsPropertyOf[K, Init || PropName :: PropType]): F[i.Mod, i.Type] =
      i.switch[F[i.Mod, i.Type]](
        caseLastProp =
          [init] => (
            ev1: (Init || PropName :: PropType) =:= (init || K :: i.Type),
            ev2: TypeEqK[i.Modality, [A] =>> A],
            ev3: i.Mod =:= Mod.Required.type,
          ) => {
            ev1 match
              case BiInjective[||](_, BiInjective[::](_, TypeEq(Refl()))) =>
                TypeEq(ev3.flip).substUpperBounded[Mod, [x <: Mod] =>> F[x, i.Type]](pval)
          },
        caseOptLastProp =
          [init] => (
            ev1: (Init || PropName :: PropType) =:= (init || K :? i.Type),
            ev2: TypeEqK[i.Modality, Option],
            ev3: i.Mod =:= ObjectMotif.Mod.Optional.type,
          ) => {
            ev1 match
              case BiInjective[||](_, ev) => :?.isNot_::[K, i.Type, PropName, PropType](using ev.flip)
          },
        caseInitProp =
          [init, last] => (
            ev1: (Init || PropName :: PropType) =:= (init || last),
            j:   IsPropertyOf.Aux[K, init, i.Type, i.Modality, i.Mod],
          ) => {
            ev1 match
              case BiInjective[||](TypeEq(Refl()), _) =>
                init.get[K](using j)
          },
      )

    override def traverse[M[_], G[_ <: Mod,_]](
      f: [A] => F[Mod.Required.type, A] => M[G[Mod.Required.type, A]],
      g: [A] => F[Mod.Optional.type, A] => M[G[Mod.Optional.type, A]],
    )(using
      M: Applicative[M],
    ): M[ObjectMotif[G, Init || PropName :: PropType]] =
      M.map2(
        init.traverse(f, g),
        f(pval)
      ): (init, pval) =>
        Snoc(init, pname, pval)

    override def translate[G[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[M, A],
    ): ObjectMotif[G, Init || PropName :: PropType] =
      Snoc(init.translate(f), pname, f(pval))

    override def wipeTranslate[H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => Exists[[X] =>> H[M, X]],
    ): Exists[[X] =>> ObjectMotif[H, X]] =
      Indeed(Snoc(init.wipeTranslate(f).value, pname, f(pval).value))

    override def wipeTranslateA[G[_], H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[Exists[[X] =>> H[M, X]]],
    )(using
      G: Applicative[G],
    ): G[Exists[[X] =>> ObjectMotif[H, X]]] =
      G.map2(
        init.wipeTranslateA(f),
        f(pval),
      ): (init, pval) =>
        Indeed(Snoc(init.value, pname, pval.value))

    override protected def zipWithNamedTupleAcc[G[_], Acc <: AnyNamedTuple, H[_ <: Mod,_]](
      t: PropsToNamedTupleAcc[G, Init || PropName :: PropType, Acc],
    )(
      fReq: [A] => (F[Required.type, A], G[A]) => H[Required.type, A],
      fOpt: [A] => (F[Optional.type, A], G[A] | None.type) => H[Optional.type, A],
    ): (ObjectMotif[H, Init || PropName :: PropType], Acc) = {
      type Acc1 =
        NamedTuples.Cons[PropName, G[PropType], Acc]
      val (initH, acc1) =
        init.zipWithNamedTupleAcc(
          t: PropsToNamedTupleAcc[G, Init, Acc1],
        )(fReq, fOpt)
      val g: G[PropType] =
        acc1.head
      val h: H[Required.type, PropType] =
        fReq(pval, g)
      val acc: Acc =
        (acc1.tail : NamedTuple[Names[Acc], DropNames[Acc]]) // clearly Acc, but does not reduce to Acc
          .asInstanceOf[Acc]
      (Snoc(initH, pname, h), acc)
    }

    override def zipWithNamedTuple2Acc[G[_], NAcc <: Tuple, TAcc <: Tuple, H[_ <: Mod,_]](
      t: NamedTuple[PropNamesTupleAcc[Init || PropName :: PropType, NAcc], PropTypesTupleAcc[G, Init || PropName :: PropType, TAcc]],
    )(
      fReq: [A] => (F[Required.type, A], G[A]) => H[Required.type, A],
      fOpt: [A] => (F[Optional.type, A], G[A] | None.type) => H[Optional.type, A],
    ): (ObjectMotif[H, Init || PropName :: PropType], TAcc) = {
      type NAcc1 =   PropName  *: NAcc
      type TAcc1 = G[PropType] *: TAcc
      val (initH, acc1) =
        init.zipWithNamedTuple2Acc[G, NAcc1, TAcc1, H](
          t: NamedTuple[PropNamesTupleAcc[Init, NAcc1], PropTypesTupleAcc[G, Init, TAcc1]]
        )(fReq, fOpt)
      val g: G[PropType] =
        acc1.head
      val h: H[Required.type, PropType] =
        fReq(pval, g)
      val acc: TAcc =
        acc1.tail
      (Snoc(initH, pname, h), acc)
    }

  }

  case class SnocOpt[F[_ <: Mod, _], Init, PropName <: String, PropType](
    init: ObjectMotif[F, Init],
    pname: SingletonType[PropName],
    pval: F[Mod.Optional.type, PropType],
  ) extends ObjectMotif.NonEmpty[F, Init || PropName :? PropType] {

    override def get[K](using i: IsPropertyOf[K, Init || PropName :? PropType]): F[i.Mod, i.Type] =
      i.switch[F[i.Mod, i.Type]](
        caseLastProp =
          [init] => (
            ev1: (Init || PropName :? PropType) =:= (init || K :: i.Type),
            ev2: TypeEqK[i.Modality, [A] =>> A],
            ev3: i.Mod =:= Mod.Required.type,
          ) => {
            ev1 match
              case BiInjective[||](_, ev) =>
                :?.isNot_::(using ev)
          },
        caseOptLastProp =
          [init] => (
            ev1: (Init || PropName :? PropType) =:= (init || K :? i.Type),
            ev2: TypeEqK[i.Modality, Option],
            ev3: i.Mod =:= Mod.Optional.type,
          ) => {
            ev1 match
              case BiInjective[||](_, BiInjective[:?](_, TypeEq(Refl()))) =>
                TypeEq(ev3.flip).substUpperBounded[Mod, [x <: Mod] =>> F[x, i.Type]](pval)
          },
        caseInitProp =
          [init, last] => (
            ev1: (Init || PropName :? PropType) =:= (init || last),
            j:   IsPropertyOf.Aux[K, init, i.Type, i.Modality, i.Mod],
          ) => {
            ev1 match
              case BiInjective[||](TypeEq(Refl()), _) =>
                init.get[K](using j)
          },
      )

    override def traverse[M[_], G[_ <: Mod,_]](
      f: [A] => F[Mod.Required.type, A] => M[G[Mod.Required.type, A]],
      g: [A] => F[Mod.Optional.type, A] => M[G[Mod.Optional.type, A]],
    )(using
      M: Applicative[M],
    ): M[ObjectMotif[G, Init || PropName :? PropType]] =
      M.map2(
        init.traverse(f, g),
        g(pval),
      ): (init, pval) =>
        SnocOpt(init, pname, pval)

    override def translate[G[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[M, A],
    ): ObjectMotif[G, Init || PropName :? PropType] =
      SnocOpt(init.translate(f), pname, f(pval))

    override def wipeTranslate[H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => Exists[[X] =>> H[M, X]],
    ): Exists[[X] =>> ObjectMotif[H, X]] =
      Indeed(SnocOpt(init.wipeTranslate(f).value, pname, f(pval).value))

    override def wipeTranslateA[G[_], H[_ <: Mod,_]](
      f: [M <: Mod, A] => F[M, A] => G[Exists[[X] =>> H[M, X]]],
    )(using
      G: Applicative[G],
    ): G[Exists[[X] =>> ObjectMotif[H, X]]] =
      G.map2(
        init.wipeTranslateA(f),
        f(pval),
      ): (init, pval) =>
        Indeed(SnocOpt(init.value, pname, pval.value))

    override protected def zipWithNamedTupleAcc[G[_], Acc <: AnyNamedTuple, H[_ <: Mod,_]](
      t: PropsToNamedTupleAcc[G, Init || PropName :? PropType, Acc],
    )(
      fReq: [A] => (F[Required.type, A], G[A]) => H[Required.type, A],
      fOpt: [A] => (F[Optional.type, A], G[A] | None.type) => H[Optional.type, A],
    ): (ObjectMotif[H, Init || PropName :? PropType], Acc) ={
      type Acc1 =
        NamedTuples.Cons[PropName, G[PropType] | None.type, Acc]
      val (initH, acc1) =
        init.zipWithNamedTupleAcc(
          t: PropsToNamedTupleAcc[G, Init, Acc1],
        )(fReq, fOpt)
      val g: G[PropType] | None.type =
        acc1.head
      val h: H[Optional.type, PropType] =
        fOpt(pval, g)
      val acc: Acc =
        (acc1.tail : NamedTuple[Names[Acc], DropNames[Acc]]) // clearly Acc, but does not reduce to Acc
          .asInstanceOf[Acc]
      (SnocOpt(initH, pname, h), acc)
    }

    override def zipWithNamedTuple2Acc[G[_], NAcc <: Tuple, TAcc <: Tuple, H[_ <: Mod,_]](
      t: NamedTuple[PropNamesTupleAcc[Init || PropName :? PropType, NAcc], PropTypesTupleAcc[G, Init || PropName :? PropType, TAcc]],
    )(
      fReq: [A] => (F[Required.type, A], G[A]) => H[Required.type, A],
      fOpt: [A] => (F[Optional.type, A], G[A] | None.type) => H[Optional.type, A],
    ): (ObjectMotif[H, Init || PropName :? PropType], TAcc) = {
      type NAcc1 =   PropName  *: NAcc
      type TAcc1 = (G[PropType] | None.type) *: TAcc
      val (initH, acc1) =
        init.zipWithNamedTuple2Acc[G, NAcc1, TAcc1, H](
          t: NamedTuple[PropNamesTupleAcc[Init, NAcc1], PropTypesTupleAcc[G, Init, TAcc1]]
        )(fReq, fOpt)
      val g: G[PropType] | None.type =
        acc1.head
      val h: H[Optional.type, PropType] =
        fOpt(pval, g)
      val acc: TAcc =
        acc1.tail
      (SnocOpt(initH, pname, h), acc)
    }

  }
}
