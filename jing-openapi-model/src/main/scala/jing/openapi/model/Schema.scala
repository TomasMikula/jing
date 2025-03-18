package jing.openapi.model

import libretto.lambda.util.SingletonValue

enum Schema[A] {
  case Proper(value: Schematic[Schema, A])

  case Unknown[S <: String](reason: SingletonValue[S]) extends Schema[Oops[S]]
}

object Schema {
  import jing.openapi.model.{Schematic as tic}

  def i64: Schema[Int64] = Proper(tic.I64())
  def str: Schema[Str]   = Proper(tic.S())

  def arr[A](sa: Schema[A]): Schema[Arr[A]] =
    Proper(tic.Array(sa))

  def objectEmpty: Schema[Obj[{}]] =
    Proper(tic.Object.Empty())

  def objectSnoc[Init, PropName <: String, PropType](
    init: Schema[Obj[Init]],
    pname: SingletonValue[PropName],
    ptype: Schema[PropType],
  ): Schema[Obj[Init || PropName :: PropType]] =
    Proper(tic.Object.Snoc(init, pname, ptype))

  def unknown[S <: String](reason: SingletonValue[S]): Schema[Oops[S]] =
    Unknown(reason)

  def unknown(reason: String): Schema[Oops[reason.type]] =
    unknown(SingletonValue(reason))

  def asObject[Ps](s: Schema[Obj[Ps]]): Schematic.Object[Schema, Ps] =
    s match
      case Proper(value) =>
        Schematic.asObject(value)
      case u: Unknown[rsn] =>
        throw AssertionError(s"Impossible for Obj[X] =:= Oops[Y], as `Obj` and `Oops` are distinct class types")

}
