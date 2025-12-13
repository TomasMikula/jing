package jing.openapi.model

import libretto.lambda.util.{ClampEq, SingletonType}

enum Schema[A] {
  case Proper(value: SchemaMotif[Schema, A])

  case Unsupported[S <: String](message: SingletonType[S]) extends Schema[Oops[S]]

  infix def isEqualTo[B](that: Schema[B]): Option[A =:= B] =
    summon[ClampEq[Schema]].testEqual(this, that)
}

object Schema {
  import jing.openapi.model.{SchemaMotif as motif}

  def i64: Schema[Int64] = Proper(motif.I64())
  def str: Schema[Str]   = Proper(motif.S())

  def arr[A](sa: Schema[A]): Schema[Arr[A]] =
    Proper(motif.Array(sa))

  def objectEmpty: Schema[Obj[Void]] =
    Proper(motif.Object.empty)

  def objectSnoc[Init, PropName <: String, PropType](
    init: Schema[Obj[Init]],
    pname: SingletonType[PropName],
    ptype: Schema[PropType],
  ): Schema[Obj[Init || PropName :: PropType]] =
    Proper(motif.Object.snoc(asObject(init), pname, ptype))

  def unsupported[S <: String](message: SingletonType[S]): Schema[Oops[S]] =
    Unsupported(message)

  def unsupported(message: String): Schema[Oops[message.type]] =
    unsupported(SingletonType(message))

  extension [Ps](s: Schema[Obj[Ps]]) {
    def asObject: SchemaMotif.Object[Schema, Ps] =
      s match
        case Proper(value) =>
          value.asObject

    def propertyList: PropertyList[Ps] =
      s match
        case Proper(value) =>
          value.propertyList
  }

  enum Labeled[L, A] {
    case WithLabel(label: L, schema: Schema.Labeled[L, A])

    case Unlabeled(value: SchemaMotif[Schema.Labeled[L, _], A])

    case Unsupported[L, S <: String](message: SingletonType[S]) extends Schema.Labeled[L, Oops[S]]

    def labelOpt: Option[L] =
      this match
        case WithLabel(label, schema) => Some(label)
        case Unlabeled(value) => None
        case Labeled.Unsupported(message) => None

    infix def isEqualTo[B](that: Schema.Labeled[L, B]): Option[A =:= B] =
      summon[ClampEq[Schema.Labeled[L, _]]].testEqual(this, that)

    def stripLabels: Schema[A] =
      this match
        case WithLabel(label, schema) => schema.stripLabels
        case Unlabeled(value) => Schema.Proper(value.translate([X] => _.stripLabels))
        case Labeled.Unsupported(message) => Schema.Unsupported(message)

  }

  object Labeled {
    def unsupported[L](message: String): Schema.Labeled[L, Oops[message.type]] =
      Labeled.Unsupported(SingletonType(message))

    given [L] => ClampEq[Schema.Labeled[L, _]] =
      new ClampEq[Schema.Labeled[L, _]] { self =>
        override def testEqual[A, B](a: Labeled[L, A], b: Labeled[L, B]): Option[A =:= B] =
          (a, b) match
            case (Unlabeled(a), Unlabeled(b)) =>
              (a isEqualTo b)(using self)
            case (WithLabel(la, a), WithLabel(lb, b)) =>
              if (la == lb)
                then testEqual(a, b)
                else None
            case (Unsupported(a), Unsupported(b)) =>
              SingletonType.testEqualString(a, b).map(_.liftCo[Oops])
            case _ =>
              None
      }
  }

  given ClampEq[Schema] with { self =>

    override def testEqual[A, B](a: Schema[A], b: Schema[B]): Option[A =:= B] =
      (a, b) match
        case (Proper(a), Proper(b)) =>
          (a isEqualTo b)(using self)
        case (Unsupported(a), Unsupported(b)) =>
          SingletonType.testEqualString(a, b).map(_.liftCo[Oops])
        case _ =>
          None

  }
}
