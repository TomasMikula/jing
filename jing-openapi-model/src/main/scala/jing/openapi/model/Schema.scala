package jing.openapi.model

import libretto.lambda.util.SingletonType

enum Schema[A] {
  case Proper(value: SchemaMotif[Schema, A])

  case Unsupported[S <: String](message: SingletonType[S]) extends Schema[Oops[S]]
}

object Schema {
  import jing.openapi.model.{SchemaMotif as motif}

  def i64: Schema[Int64] = Proper(motif.I64())
  def str: Schema[Str]   = Proper(motif.S())

  def arr[A](sa: Schema[A]): Schema[Arr[A]] =
    Proper(motif.Array(sa))

  def objectEmpty: Schema[Obj[Void]] =
    Proper(motif.Object.Empty())

  def objectSnoc[Init, PropName <: String, PropType](
    init: Schema[Obj[Init]],
    pname: SingletonType[PropName],
    ptype: Schema[PropType],
  ): Schema[Obj[Init || PropName :: PropType]] =
    Proper(motif.Object.Snoc(asObject(init), pname, ptype))

  def unsupported[S <: String](message: SingletonType[S]): Schema[Oops[S]] =
    Unsupported(message)

  def unsupported(message: String): Schema[Oops[message.type]] =
    unsupported(SingletonType(message))

  def asObject[Ps](s: Schema[Obj[Ps]]): SchemaMotif.Object[Schema, Ps] =
    s match
      case Proper(value) =>
        SchemaMotif.asObject(value)
      case u: Unsupported[msg] =>
        throw AssertionError(s"Impossible for Obj[X] =:= Oops[Y], as `Obj` and `Oops` are distinct class types")

  object Object {
    opaque type NonEmpty[Props] =
      SchemaMotif.Object.NonEmpty[Schema, Props]

    object NonEmpty {
      def fromMotif[Props](motif: SchemaMotif.Object.NonEmpty[Schema, Props]): NonEmpty[Props] =
        motif

      extension [Props](o: Object.NonEmpty[Props])
        def toMotif: SchemaMotif.Object.NonEmpty[Schema, Props] =
          o
    }
  }
}
