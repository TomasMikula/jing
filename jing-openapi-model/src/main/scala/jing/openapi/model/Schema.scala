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

  def asObject[Ps](s: Schema[Obj[Ps]]): SchemaMotif.Object[Schema, Ps] =
    s match
      case Proper(value) =>
        SchemaMotif.asObject(value)
}
