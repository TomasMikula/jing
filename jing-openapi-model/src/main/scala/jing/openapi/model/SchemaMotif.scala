package jing.openapi.model

import libretto.lambda.util.TypeEq.Refl
import libretto.lambda.util.{Applicative, ClampEq, Exists, SingletonType, TypeEq}
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.{Items1, Items1Named}

/** Schema structure parametric in the type of nested schemas.
 *
 * @tparam F representation of nested schemas. In [[Schema]],
 *   [[SchemMotif]] is used with `F` recursively instantiated to [[Schema]] itself.
 */
sealed trait SchemaMotif[F[_], A] {
  import SchemaMotif.*

  def translate[G[_]](h: [X] => F[X] => G[X]): SchemaMotif[G, A] =
    this match
      case p: Primitive[F, A] => p.recast[G]
      case Constant.Primitive(v) => Constant.Primitive(v)
      case Array(elem) => Array(h(elem))
      case Object(value) => Object:
        value.translate[G, G](
          [A] => fa => h(fa),
          [A] => fa => h(fa),
        )
      case OneOf(discriminator, schemas) =>
        OneOf(discriminator, schemas.translate(h))

  def relateTranslate[Rel[_, _], G[_]](
    h: [X] => F[X] => Exists[[Y] =>> (Rel[X, Y], G[Y])],
  )(using
    Rel: Compatible[Rel],
  ): Exists[[B] =>> (Rel[A, B], SchemaMotif[G, B])] =
    relateTranslateA[Rel, G, [x] =>> x](h)

  def relateTranslateA[Rel[_, _], G[_], M[_]](
    h: [X] => F[X] => M[Exists[[Y] =>> (Rel[X, Y], G[Y])]],
  )(using
    Rel: Compatible[Rel],
    M: Applicative[M],
  ): M[Exists[[B] =>> (Rel[A, B], SchemaMotif[G, B])]] =
    this match
      case p: Primitive[F, A] =>
        M.pure(Indeed((p.refl[Rel], p.recast[G])))
      case c @ Constant.Primitive(v) =>
        M.pure(Indeed((c.refl[Rel], Constant.Primitive(v))))
      case Array(elem) =>
        h(elem).map:
          case Indeed((rel, ga)) =>
            Indeed((rel.lift_arr, Array(ga)))
      case Object(value) =>
        value.relateTranslateA[Rel, G, G, M](
          [A] => fa => h(fa),
          [A] => fa => h(fa),
        ).map:
          case Indeed((rel, obj)) =>
            Indeed((rel.lift_obj, Object(obj)))
      case OneOf(discriminator, schemas) =>
        schemas.relateTranslateA(h)(
          labelRelated = [K <: String, X, Y] => (k: SingletonType[K], xRy: X `Rel` Y) => xRy.lift_-::-[K](using k),
          snocRelated = [X1, X2, Y1, Y2] => Rel.lift_||(_, _),
        ).map:
          case Indeed((rel, gSchemas)) =>
            Indeed((rel.lift_discriminatedUnion, OneOf(discriminator, gSchemas)))

  def isNotOops[S](using A =:= Oops[S]): Nothing =
    throw AssertionError("Impossible: Schemas for type Oops[S] are not representable by SchemaMotif")

  infix def isEqualTo[B](that: SchemaMotif[F, B])(using F: ClampEq[F]): Option[A =:= B] =
    (this, that) match
      case (I32(), I32()) =>
        Some(summon)
      case (I64(), I64()) =>
        Some(summon)
      case (S(), S()) =>
        Some(summon)
      case (B(), B()) =>
        Some(summon)
      case (x: Enumeration[f, t, as], y: Enumeration[g, u, bs]) =>
        (x.baseType isEqualTo y.baseType) flatMap:
          case TypeEq(Refl()) =>
            (x.cases isEqualTo y.cases) map:
              case TypeEq(Refl()) =>
                summon[Enum[t, as] =:= Enum[u, bs]]
      case (Constant.Primitive(x), Constant.Primitive(y)) =>
        (x isEqualTo y).map(_._1.liftCo[Const])
      case (Array(x), Array(y)) =>
        F.testEqual(x, y).map(_.liftCo[Arr])
      case (Object(x), Object(y)) =>
        (x isEqualTo y).map(_.liftCo[Obj])
      case (OneOf(p, xs), OneOf(q, ys)) =>
        Option
          .when(p == q) { xs isEqualTo ys }
          .flatten
          .map(_.liftCo[DiscriminatedUnion])
      case _ =>
        None
}

object SchemaMotif {
  sealed trait Primitive[F[_], T] extends SchemaMotif[F, T] {
    def recast[G[_]]: Primitive[G, T] = this.asInstanceOf
    def refl[Rel[_, _]](using Compatible[Rel]): Rel[T, T]
  }

  sealed trait BasicPrimitive[F[_], T] extends Primitive[F, T] {
    def makeValue(v: ScalaReprOf[T]): Value[T] =
      this match
        case I32() => Value.int32(v)
        case I64() => Value.int64(v)
        case S() => Value.str(v)
        case B() => Value.bool(v)

    override def refl[Rel[_, _]](using Rel: Compatible[Rel]): Rel[T, T] =
      this match
        case I32() => Rel.lift_int32
        case I64() => Rel.lift_int64
        case S()   => Rel.lift_str
        case B()   => Rel.lift_bool
  }

  case class I32[F[_]]() extends BasicPrimitive[F, Int32]
  case class I64[F[_]]() extends BasicPrimitive[F, Int64]
  case class S[F[_]]() extends BasicPrimitive[F, Str]
  case class B[F[_]]() extends BasicPrimitive[F, Bool]

  case class Enumeration[F[_], T, Cases](
    baseType: BasicPrimitive[F, T],
    cases: Items1.Product[||, Void, ScalaValueOf[_, T], Cases],
  ) extends Primitive[F, Enum[T, Cases]] {

    override def refl[Rel[_, _]](using Rel: Compatible[Rel]): Rel[Enum[T, Cases], Enum[T, Cases]] =
      val rCases: Cases `Rel` Cases =
        cases.foldMap[[x] =>> Rel[x, x]](
          baseCase = [X] => x => Rel.lift_||(Rel.lift_void, x.refl[Rel]),
          snocCase = [Init, X] => (rInit, x) => Rel.lift_||(rInit, x.refl[Rel]),
        )
      Rel.lift_enum(
        baseType.refl[Rel],
        rCases,
      )

    def find(a: ScalaReprOf[T]): Option[ScalaValueOf[? <: a.type & ScalaUnionOf[Cases], T]] = {

      def go[Cases](
        cases: Items1.Product[||, Void, ScalaValueOf[_, T], Cases],
      ): Option[ScalaValueOf[? <: a.type & ScalaUnionOf[Cases], T]] =
        import libretto.lambda.Items1.Product.{Single, Snoc}

        cases match
          case s: Single[sep, nil, f, c] =>
            val ev: c =:= ScalaUnionOf[Cases] =
              summon[(Void || c) =:= Cases].liftCo[ScalaUnionOf]
            ev.substituteCo[[x] =>> Option[ScalaValueOf[a.type & x, T]]]:
              s.value.contains(a)
          case s: Snoc[sep, nil, f, init, last] =>
            val ev: last <:< ScalaUnionOf[Cases] =
              summon[(init || last) =:= Cases].liftCo[ScalaUnionOf]

            ev.substituteCo[[x] =>> Option[ScalaValueOf[? <: a.type & x, T]]]:
              s.last.contains(a)
            .orElse:
              val ev2: ScalaUnionOf[init] <:< ScalaUnionOf[Cases] =
                summon[(init || last) =:= Cases].liftCo[ScalaUnionOf]
              ev2.substituteCo[[x] =>> Option[ScalaValueOf[? <: a.type & x, T]]]:
                go[init](s.init)

      go(cases)
    }

    def scalaValues: List[ScalaReprOf[T]] =
      cases.toList([a] => (va: ScalaValueOf[a, T]) => va.get)
  }

  object Enumeration {
    def str[F[_]](value: String, values: String*): Enumeration[F, Str, ?] =
      apply(SchemaMotif.S(), ScalaValueOf.str, value, values*)

    def int32[F[_]](value: Int, values: Int*): Enumeration[F, Int32, ?] =
      apply(SchemaMotif.I32(), ScalaValueOf.i32, value, values*)

    def int64[F[_]](value: Long, values: Long*): Enumeration[F, Int64, ?] =
      apply(SchemaMotif.I64(), ScalaValueOf.i64, value, values*)

    def bool[F[_]](value: Boolean, values: Boolean*): Enumeration[F, Bool, ?] =
      apply(SchemaMotif.B(), ScalaValueOf.bool, value, values*)

    private def apply[F[_], Base, X](
      s: SchemaMotif.BasicPrimitive[F, Base],
      f: (x: X) => ScalaValueOf[x.type, Base],
      x0: X,
      xs: X*
    ): Enumeration[F, Base, ?] = {
      val values1: Items1.Product[||, Void, ScalaValueOf[_, Base], ?] =
        xs.foldLeft[Items1.Product[||, Void, ScalaValueOf[_, Base], ?]](
          Items1.Product.Single(f(x0))
        ) { (acc, x) =>
          Items1.Product.Snoc(acc, f(x))
        }

      SchemaMotif.Enumeration(s, values1)
    }
  }

  sealed trait Constant[F[_], T] extends SchemaMotif[F, Const[T]]

  object Constant {
    case class Primitive[F[_], T](value: T ScalaValueOf ?) extends Constant[F, T] {
      def refl[Rel[_, _]](using Compatible[Rel]): Rel[Const[T], Const[T]] =
        value.refl[Rel].lift_const
    }
  }

  case class Array[F[_], T](elem: F[T]) extends SchemaMotif[F, Arr[T]]

  case class Object[F[_], Ps](
    value: ObjectMotif[F, F, Ps],
  ) extends SchemaMotif[F, Obj[Ps]] {
    def propertyList: PropertyList[Ps] =
      PropertyList.from(value)
  }

  object Object {
    def empty[F[_]]: Object[F, Void] =
      Object(ObjectMotif.Empty())

    def snoc[F[_], Init, K <: String, V](
      init: SchemaMotif[F, Obj[Init]],
      pname: SingletonType[K],
      ptype: F[V],
    ): Object[F, Init || K :: V] =
      Object(ObjectMotif.Snoc(asObject(init).value, pname, ptype))

    def snoc[F[_], Init, PropType](
      init: SchemaMotif[F, Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Object[F, Init || pname.type :: PropType] =
      snoc(init, SingletonType(pname), ptype)

    def snocOpt[F[_], Init, K <: String, V](
      init: SchemaMotif[F, Obj[Init]],
      pname: SingletonType[K],
      ptype: F[V],
    ): Object[F, Init || K :? V] =
      Object(ObjectMotif.SnocOpt(asObject(init).value, pname, ptype))

    def snocOpt[F[_], Init, PropType](
      init: SchemaMotif[F, Obj[Init]],
      pname: String,
      ptype: F[PropType],
    ): Object[F, Init || pname.type :? PropType] =
      snocOpt(init, SingletonType(pname), ptype)
  }

  extension [F[_], Ps](s: SchemaMotif[F, Obj[Ps]])
    def asObject: SchemaMotif.Object[F, Ps] =
      s match
        case o: Object[F, Ps] => o

    def propertyList: PropertyList[Ps] =
      asObject.propertyList

  case class OneOf[F[_], Cases](
    discriminatorProperty: String,
    schemas: Items1Named.Product[||, ::, F, Cases],
  ) extends SchemaMotif[F, DiscriminatedUnion[Cases]]

  given [F[_]] => ClampEq[F] => ClampEq[SchemaMotif[F, _]] =
    new ClampEq[SchemaMotif[F, _]]:
      override def testEqual[A, B](a: SchemaMotif[F, A], b: SchemaMotif[F, B]): Option[A =:= B] =
        a isEqualTo b
}
