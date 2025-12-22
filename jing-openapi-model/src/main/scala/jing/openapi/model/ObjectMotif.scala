package jing.openapi.model

import jing.openapi.model.IsPropertyOf.{IsOptionalPropertyOf, IsRequiredPropertyOf}
import libretto.lambda.util.{Applicative, ClampEq, Exists, SingletonType, TypeEq, TypeEqK}
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq.Refl

import scala.NamedTuple.NamedTuple

sealed trait ObjectMotif[Req[_], Opt[_], Props] {
  import ObjectMotif.*

  def get[K](using i: IsPropertyOf[K, Props]): i.ReqOrOpt[Req, Opt][i.Type] =
    i.switch[i.ReqOrOpt[Req, Opt][i.Type]](
      caseLastProp =
        [init] => (
          ev1: Props =:= (init || K :: i.Type),
          ev2: [F[_], G[_]] => DummyImplicit ?=> TypeEqK[i.ReqOrOpt[F, G], F],
        ) => {
          val value: Req[i.Type] = ev1.substituteCo(this).unsnocReq._3
          ev2[Req, Opt].flip.at[i.Type](value)
        },
      caseOptLastProp =
        [init] => (
          ev1: Props =:= (init || K :? i.Type),
          ev2: [F[_], G[_]] => DummyImplicit ?=> TypeEqK[i.ReqOrOpt[F, G], G],
        ) => {
          val value: Opt[i.Type] = ev1.substituteCo(this).unsnocOpt._3
          ev2[Req, Opt].flip.at[i.Type](value)
        },
      caseInitProp =
        [init, last] => (
          ev1: Props =:= (init || last),
          j:   IsPropertyOf.Aux[K, init, i.Type, i.ReqOrOpt],
        ) => {
          val init: ObjectMotif[Req, Opt, init] =
            ev1.substituteCo(this).unsnoc._1
          val ev: j.ReqOrOpt[Req, Opt][j.Type] =:= i.ReqOrOpt[Req, Opt][i.Type] =
            summon[TypeEqK[j.ReqOrOpt[Req, Opt], i.ReqOrOpt[Req, Opt]]]
              .atH[j.Type, i.Type]
          ev(init.get(using j))
        },
    )

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
  ): M[ObjectMotif[G, H, Props]] =
    relateTranslateA[=:=, G, H, M](
      [X] => rx => g(rx).map(gx => Indeed((summon[X =:= X], gx))),
      [X] => ox => h(ox).map(hx => Indeed((summon[X =:= X], hx))),
    ).map:
      case Indeed((TypeEq(Refl()), res)) => res

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

  case class Snoc[Req[_], Opt[_], Init, P](
    init: ObjectMotif[Req, Opt, Init],
    last: Property[Req, Opt, P],
  ) extends ObjectMotif[Req, Opt, Init || P] {

    override def getOpt(k: String): Option[Exists[[A] =>> Req[A] | Opt[A]]] =
      last.getOpt(k) `orElse` init.getOpt(k)

    override def getOptFull(k: String): Option[Exists[[A] =>>
      Either[
        (IsRequiredPropertyOf.Aux[k.type, Init || P, A], Req[A]),
        (IsOptionalPropertyOf.Aux[k.type, Init || P, A], Opt[A]),
      ]
    ]] =
      SingletonType.testEqualString(SingletonType(k), last.name) match
        case Some(ev) =>
          last.asLastPropertyOf[Init] match
            case Left((isReqProp, ra)) =>
              val i: IsRequiredPropertyOf.Aux[k.type, Init || P, last.Type] =
                ev.substituteContra[IsRequiredPropertyOf.Aux[_, Init || P, last.Type]](isReqProp)
              Some(Indeed(Left((i, ra))))
            case Right((isOptProp, oa)) =>
              val i: IsOptionalPropertyOf.Aux[k.type, Init || P, last.Type] =
                ev.substituteContra[IsOptionalPropertyOf.Aux[_, Init || P, last.Type]](isOptProp)
              Some(Indeed(Right((i, oa))))
        case None =>
          init
            .getOptFull(k)
            .map:
              case Indeed(Left((i, v))) => Indeed(Left((i.inInit, v)))
              case Indeed(Right((i, v))) => Indeed(Right((i.inInit, v)))

    override def isEqualTo[Qrops](that: ObjectMotif[Req, Opt, Qrops])(using
      ClampEq[Req],
      ClampEq[Opt],
    ): Option[(Init || P) =:= Qrops] =
      that match
        case that: Snoc[req, opt, init, p] =>
          for
            evInit <- this.init isEqualTo that.init
            evLast <- this.last isEqualTo that.last
          yield
            evInit.liftCo[||[_, P]] andThen evLast.liftCo[||[init, _]]
        case Empty() =>
          None

    override def relateTranslateA[Rel[_,_], G[_], H[_], M[_]](
      g: [X] => Req[X] => M[Exists[[Y] =>> (Rel[X, Y], G[Y])]],
      h: [X] => Opt[X] => M[Exists[[Y] =>> (Rel[X, Y], H[Y])]],
    )(using
      Rel: Compatible[Rel],
      M: Applicative[M],
    ): M[Exists[[Qs] =>> (Rel[Init || P, Qs], ObjectMotif[G, H, Qs])]] =
      M.map2(
        init.relateTranslateA(g, h),
        last.relateTranslateA(g, h),
      ):
        case (Indeed(ri, init), Indeed(rl, last)) =>
          val rel = Rel.lift_||(ri, rl)
          Indeed((rel, Snoc(init, last)))

    override protected def toTupleAcc[G[_], H[_], TAcc <: Tuple](
      g: [A] => Req[A] => G[A],
      h: [A] => Opt[A] => H[A],
      acc: TAcc,
    ): PropTypesTupleFAcc[G, H, Init || P, TAcc] =
      init.toTupleAcc[G, H, PropTypeF[G, H, P] *: TAcc](g, h, last.value(g, h) *: acc)

    override protected def zipWithNamedTupleAcc[G[_], H[_], NAcc <: Tuple, TAcc <: Tuple, I[_], J[_]](
      t: NamedTuple[
        PropNamesTupleAcc[Init, PropName[P] *: NAcc],
        PropTypesTupleFAcc[G, H, Init, PropTypeF[G, H, P] *: TAcc]
      ],
    )(
      fReq: [A] => (Req[A], G[A]) => I[A],
      fOpt: [A] => (Opt[A], H[A]) => J[A],
    ): (ObjectMotif[I, J, Init || P], TAcc) =
      type NAcc1 = PropName[P]        *: NAcc
      type TAcc1 = PropTypeF[G, H, P] *: TAcc
      val (initH, acc1) =
        init.zipWithNamedTupleAcc[G, H, NAcc1, TAcc1, I, J](
          t: NamedTuple[PropNamesTupleAcc[Init, NAcc1], PropTypesTupleFAcc[G, H, Init, TAcc1]]
        )(fReq, fOpt)
      val ghp: PropTypeF[G, H, P] =
        acc1.head
      val ijp: Property[I, J, P] =
        last.zipWith(ghp)(fReq, fOpt)
      val acc: TAcc =
        acc1.tail
      (Snoc(initH, ijp), acc)

  }

  sealed trait Property[Req[_], Opt[_], P] {
    type Name <: String
    type Type

    def name: SingletonType[Name]
    def value: Req[Type] | Opt[Type]

    def value[G[_], H[_]](
      g: [A] => Req[A] => G[A],
      h: [A] => Opt[A] => H[A],
    ): PropTypeF[G, H, P]

    def getOpt(k: String): Option[Exists[[A] =>> Req[A] | Opt[A]]] =
      Option.when(name.value == k):
        Exists(value)

    infix def isEqualTo[Q](that: Property[Req, Opt, Q])(using
      Req: ClampEq[Req],
      Opt: ClampEq[Opt],
    ): Option[P =:= Q]

    def relateTranslateA[Rel[_,_], G[_], H[_], M[_]](
      g: [X] => Req[X] => M[Exists[[Y] =>> (Rel[X, Y], G[Y])]],
      h: [X] => Opt[X] => M[Exists[[Y] =>> (Rel[X, Y], H[Y])]],
    )(using
      Rel: Compatible[Rel],
      M: Applicative[M],
    ): M[Exists[[Q] =>> (Rel[P, Q], Property[G, H, Q])]]

    def zipWith[G[_], H[_], I[_], J[_]](
      that: PropTypeF[G, H, P],
    )(
      fReq: [A] => (Req[A], G[A]) => I[A],
      fOpt: [A] => (Opt[A], H[A]) => J[A],
    ): Property[I, J, P]

    def asLastPropertyOf[Init]: Either[
      (IsRequiredPropertyOf.Aux[Name, Init || P, Type], Req[Type]),
      (IsOptionalPropertyOf.Aux[Name, Init || P, Type], Opt[Type]),
    ]
  }

  object Property {
    case class Required[Req[_], Opt[_], K <: String, T](name: SingletonType[K], value: Req[T]) extends Property[Req, Opt, K :: T] {
      override type Name = K
      override type Type = T

      override def value[G[_], H[_]](
        g: [A] => Req[A] => G[A],
        h: [A] => Opt[A] => H[A],
      ): PropTypeF[G, H, K :: T] =
        g(value)

      override def isEqualTo[Q](that: Property[Req, Opt, Q])(using
        Req: ClampEq[Req],
        Opt: ClampEq[Opt],
      ): Option[K :: T =:= Q] =
        that match
          case that: Required[req, opt, k, t] =>
            for
              evName <- SingletonType.testEqualString(this.name, that.name)
              evValue <- Req.testEqual(this.value, that.value)
            yield
              evName.liftCo[::[_, T]] andThen evValue.liftCo[::[k, _]]
          case Optional(_, _) =>
            None

      override def relateTranslateA[Rel[_,_], G[_], H[_], M[_]](
        g: [X] => Req[X] => M[Exists[[Y] =>> (Rel[X, Y], G[Y])]],
        h: [X] => Opt[X] => M[Exists[[Y] =>> (Rel[X, Y], H[Y])]],
      )(using
        Rel: Compatible[Rel],
        M: Applicative[M],
      ): M[Exists[[Q] =>> (Rel[K :: T, Q], Property[G, H, Q])]] =
        g(value).map:
          case Indeed((rel, gt)) =>
            Indeed((rel.lift_-::-[K](using name), Required(name, gt)))

      override def zipWith[G[_], H[_], I[_], J[_]](that: PropTypeF[G, H, K :: T])(
        fReq: [A] => (Req[A], G[A]) => I[A],
        fOpt: [A] => (Opt[A], H[A]) => J[A],
      ): Property[I, J, K :: T] =
        Required(name, fReq(this.value, that))

      override def asLastPropertyOf[Init]: Either[
        (IsRequiredPropertyOf.Aux[K, Init || K :: T, T], Req[T]),
        (IsOptionalPropertyOf.Aux[K, Init || K :: T, T], Opt[T]),
      ] =
        Left((summon, value))
    }

    case class Optional[Req[_], Opt[_], K <: String, T](name: SingletonType[K], value: Opt[T]) extends Property[Req, Opt, K :? T] {
      override type Name = K
      override type Type = T

      override def value[G[_], H[_]](
        g: [A] => Req[A] => G[A],
        h: [A] => Opt[A] => H[A],
      ): PropTypeF[G, H, K :? T] =
        h(value)

      override def isEqualTo[Q](that: Property[Req, Opt, Q])(using
        Req: ClampEq[Req],
        Opt: ClampEq[Opt],
      ): Option[K :? T =:= Q] =
        that match
          case that: Optional[req, opt, k, t] =>
            for
              evName <- SingletonType.testEqualString(this.name, that.name)
              evValue <- Opt.testEqual(this.value, that.value)
            yield
              evName.liftCo[:?[_, T]] andThen evValue.liftCo[:?[k, _]]
          case Required(_, _) =>
            None

      override def relateTranslateA[Rel[_,_], G[_], H[_], M[_]](
        g: [X] => Req[X] => M[Exists[[Y] =>> (Rel[X, Y], G[Y])]],
        h: [X] => Opt[X] => M[Exists[[Y] =>> (Rel[X, Y], H[Y])]],
      )(using
        Rel: Compatible[Rel],
        M: Applicative[M],
      ): M[Exists[[Q] =>> (Rel[K :? T, Q], Property[G, H, Q])]] =
        h(value).map:
          case Indeed((rel, ht)) =>
            Indeed((rel.lift_-:?-[K](using name), Optional(name, ht)))

      override def zipWith[G[_], H[_], I[_], J[_]](that: PropTypeF[G, H, K :? T])(
        fReq: [A] => (Req[A], G[A]) => I[A],
        fOpt: [A] => (Opt[A], H[A]) => J[A],
      ): Property[I, J, K :? T] =
        Optional(name, fOpt(this.value, that))

      override def asLastPropertyOf[Init]: Either[
        (IsRequiredPropertyOf.Aux[K, Init || K :? T, T], Req[T]),
        (IsOptionalPropertyOf.Aux[K, Init || K :? T, T], Opt[T]),
      ] =
        Right((summon, value))
    }

    extension [Req[_], Opt[_], K, T](p: Property[Req, Opt, K :: T])
      def nameReq: SingletonType[K] =
        p match { case Required(name, _) => name }

      def valueReq: Req[T] =
        p match { case Required(_, value) => value }

    extension [Req[_], Opt[_], K, T](p: Property[Req, Opt, K :? T])
      def nameOpt: SingletonType[K] =
        p match { case Optional(name, _) => name }

      def valueOpt: Opt[T] =
        p match { case Optional(_, value) => value }
  }

  def snocReq[Req[_], Opt[_], Init, K <: String, T](
    init: ObjectMotif[Req, Opt, Init],
    pname: SingletonType[K],
    pval: Req[T],
  ): ObjectMotif[Req, Opt, Init || K :: T] =
    Snoc(init, Property.Required(pname, pval))

  def snocOpt[Req[_], Opt[_], Init, K <: String, T](
    init: ObjectMotif[Req, Opt, Init],
    pname: SingletonType[K],
    pval: Opt[T],
  ): ObjectMotif[Req, Opt, Init || K :? T] =
    Snoc(init, Property.Optional(pname, pval))

  extension [Req[_], Opt[_], Init, K, T](obj: ObjectMotif[Req, Opt, Init || K :: T])
    def unsnocReq: (ObjectMotif[Req, Opt, Init], SingletonType[K], Req[T]) =
      obj match
        case Snoc(init, last) => (init, last.nameReq, last.valueReq)

  extension [Req[_], Opt[_], Init, K, T](obj: ObjectMotif[Req, Opt, Init || K :? T])
    def unsnocOpt: (ObjectMotif[Req, Opt, Init], SingletonType[K], Opt[T]) =
      obj match
        case Snoc(init, last) => (init, last.nameOpt, last.valueOpt)

  extension [Req[_], Opt[_], Init, P](obj: ObjectMotif[Req, Opt, Init || P])
    def nonEmpty: ObjectMotif.Snoc[Req, Opt, Init, P] =
      obj match
        case o: Snoc[r, o, i, p] => o

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
