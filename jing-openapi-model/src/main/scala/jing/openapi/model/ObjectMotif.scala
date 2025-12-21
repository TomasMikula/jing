package jing.openapi.model

import jing.openapi.model.IsPropertyOf.{IsOptionalPropertyOf, IsRequiredPropertyOf}
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq.Refl
import libretto.lambda.util.{Applicative, BiInjective, ClampEq, Exists, SingletonType, TypeEq, TypeEqK}

import scala.NamedTuple.NamedTuple

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

  infix def isEqualTo[Qrops](that: ObjectMotif[Req, Opt, Qrops])(using
    ClampEq[Req],
    ClampEq[Opt],
  ): Option[Props =:= Qrops]

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

  def relateTranslate[Rel[_, _], G[_], H[_]](
    g: [X] => Req[X] => Exists[[Y] =>> (Rel[X, Y], G[Y])],
    h: [X] => Opt[X] => Exists[[Y] =>> (Rel[X, Y], H[Y])],
  )(using
    Rel: Compatible[Rel],
  ): Exists[[Qrops] =>> (Rel[Props, Qrops], ObjectMotif[G, H, Qrops])] =
    relateTranslateA[Rel, G, H, [x] =>> x](g, h)

  def relateTranslateA[Rel[_, _], G[_], H[_], M[_]](
    g: [X] => Req[X] => M[Exists[[Y] =>> (Rel[X, Y], G[Y])]],
    h: [X] => Opt[X] => M[Exists[[Y] =>> (Rel[X, Y], H[Y])]],
  )(using
    Compatible[Rel],
    Applicative[M],
  ): M[Exists[[Qrops] =>> (Rel[Props, Qrops], ObjectMotif[G, H, Qrops])]]

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

    override def isEqualTo[Qrops](that: ObjectMotif[Req, Opt, Qrops])(using
      ClampEq[Req],
      ClampEq[Opt],
    ): Option[Void =:= Qrops] =
      that match
        case Empty() => Some(summon[Void =:= Void])
        case _ => None

    override def traverse[M[_], G[_], H[_]](
      g: [A] => Req[A] => M[G[A]],
      h: [A] => Opt[A] => M[H[A]],
    )(using
      M: Applicative[M],
    ): M[ObjectMotif[G, H, Void]] =
      M.pure(Empty())

    override def relateTranslateA[Rel[_,_], G[_], H[_], M[_]](
      g: [X] => Req[X] => M[Exists[[Y] =>> (Rel[X, Y], G[Y])]],
      h: [X] => Opt[X] => M[Exists[[Y] =>> (Rel[X, Y], H[Y])]],
    )(using
      Rel: Compatible[Rel],
      M: Applicative[M],
    ): M[Exists[[Qrops] =>> (Rel[Void, Qrops], ObjectMotif[G, H, Qrops])]] =
      M.pure(Indeed((Rel.lift_void, Empty())))

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

  sealed trait NonEmpty[Req[_], Opt[_], Init, P] extends ObjectMotif[Req, Opt, Init || P] {
    def init: ObjectMotif[Req, Opt, Init]
    def last: Property[Req, Opt, P]
  }

  case class SnocReq[Req[_], Opt[_], Init, PropName <: String, PropType](
    init: ObjectMotif[Req, Opt, Init],
    pname: SingletonType[PropName],
    pval: Req[PropType],
  ) extends ObjectMotif.NonEmpty[Req, Opt, Init, PropName :: PropType] {

    override def last: Property[Req, Opt, PropName :: PropType] =
      Property.Required(pname, pval)

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

    override def isEqualTo[Qrops](that: ObjectMotif[Req, Opt, Qrops])(using
      Req: ClampEq[Req],
      Opt: ClampEq[Opt],
    ): Option[(Init || PropName :: PropType) =:= Qrops] =
      that match
        case that: SnocReq[req, opt, init, p, t] =>
          for
            ev1 <- this.init isEqualTo that.init
            ev2 <- SingletonType.testEqualString(this.pname, that.pname)
            ev3 <- Req.testEqual(this.pval, that.pval)
          yield
            ev1.liftCo[[i] =>> i || PropName :: PropType]
              .andThen(ev2.liftCo[[p] =>> init || p :: PropType])
              .andThen(ev3.liftCo[[t] =>> init || p :: t])
        case _ =>
          None

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
        SnocReq(init, pname, pval)

    override def relateTranslateA[Rel[_,_], G[_], H[_], M[_]](
      g: [X] => Req[X] => M[Exists[[Y] =>> (Rel[X, Y], G[Y])]],
      h: [X] => Opt[X] => M[Exists[[Y] =>> (Rel[X, Y], H[Y])]],
    )(using
      Rel: Compatible[Rel],
      M: Applicative[M],
    ): M[Exists[[Qrops] =>> (Rel[Init || PropName :: PropType, Qrops], ObjectMotif[G, H, Qrops])]] =
      M.map2(
        init.relateTranslateA(g, h),
        g(pval),
      ):
        case (Indeed(ri, init), Indeed(rv, pval)) =>
          val rel = Rel.lift_||(ri, rv.lift_-::-(using pname))
          Indeed((rel, SnocReq(init, pname, pval)))

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
      (SnocReq(initH, pname, i), acc)
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
  ) extends ObjectMotif.NonEmpty[Req, Opt, Init, PropName :? PropType] {

    override def last: Property[Req, Opt, PropName :? PropType] =
      Property.Optional(pname, pval)

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

    override def isEqualTo[Qrops](that: ObjectMotif[Req, Opt, Qrops])(using
      Req: ClampEq[Req],
      Opt: ClampEq[Opt],
    ): Option[(Init || PropName :? PropType) =:= Qrops] =
      that match
        case that: SnocOpt[req, opt, init, p, t] =>
          for
            ev1 <- this.init isEqualTo that.init
            ev2 <- SingletonType.testEqualString(this.pname, that.pname)
            ev3 <- Opt.testEqual(this.pval, that.pval)
          yield
            ev1.liftCo[[i] =>> i || PropName :? PropType]
              .andThen(ev2.liftCo[[p] =>> init || p :? PropType])
              .andThen(ev3.liftCo[[t] =>> init || p :? t])
        case _ =>
          None

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

    override def relateTranslateA[Rel[_,_], G[_], H[_], M[_]](
      g: [X] => Req[X] => M[Exists[[Y] =>> (Rel[X, Y], G[Y])]],
      h: [X] => Opt[X] => M[Exists[[Y] =>> (Rel[X, Y], H[Y])]],
    )(using
      Rel: Compatible[Rel],
      M: Applicative[M],
    ): M[Exists[[Qrops] =>> (Rel[Init || PropName :? PropType, Qrops], ObjectMotif[G, H, Qrops])]] =
      M.map2(
        init.relateTranslateA(g, h),
        h(pval),
      ):
        case (Indeed((ri, init)), Indeed((rv, pval))) =>
          val rel = Rel.lift_||(ri, rv.lift_-:?-(using pname))
          Indeed((rel, SnocOpt(init, pname, pval)))

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

  sealed trait Property[Req[_], Opt[_], P]

  object Property {
    case class Required[Req[_], Opt[_], K <: String, T](k: SingletonType[K], value: Req[T]) extends Property[Req, Opt, K :: T]
    case class Optional[Req[_], Opt[_], K <: String, T](k: SingletonType[K], value: Opt[T]) extends Property[Req, Opt, K :? T]
  }

  extension [Req[_], Opt[_], Init, K, T](obj: ObjectMotif[Req, Opt, Init || K :: T])
    def unsnocReq: (ObjectMotif[Req, Opt, Init], SingletonType[K], Req[T]) =
      obj match
        case SnocReq(init, pname, pval) => (init, pname, pval)

  extension [Req[_], Opt[_], Init, K, T](obj: ObjectMotif[Req, Opt, Init || K :? T])
    def unsnocOpt: (ObjectMotif[Req, Opt, Init], SingletonType[K], Opt[T]) =
      obj match
        case SnocOpt(init, pname, pval) => (init, pname, pval)

  extension [Req[_], Opt[_], Init, P](obj: ObjectMotif[Req, Opt, Init || P])
    def nonEmpty: ObjectMotif.NonEmpty[Req, Opt, Init, P] =
      obj match
        case o: NonEmpty[r, o, i, p] => o

    def unsnoc: (ObjectMotif[Req, Opt, Init], Property[Req, Opt, P]) =
      val ne = nonEmpty
      (ne.init, ne.last)

    def lastProperty: Property[Req, Opt, P] =
      nonEmpty.last

  given [Req[_], Opt[_]] => (ClampEq[Req], ClampEq[Opt]) => ClampEq[ObjectMotif[Req, Opt, _]] =
    new ClampEq[ObjectMotif[Req, Opt, _]]:
      override def testEqual[A, B](a: ObjectMotif[Req, Opt, A], b: ObjectMotif[Req, Opt, B]): Option[A =:= B] =
        a isEqualTo b
}
