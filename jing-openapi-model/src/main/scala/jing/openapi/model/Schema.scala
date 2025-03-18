package jing.openapi.model

import libretto.lambda.util.SingletonValue

case class Schema[A](value: Schematic[Schema, A])

object Schema {
  import jing.openapi.model.{Schematic as tic}

  def i64: Schema[Int64] = Schema(tic.I64())
  def str: Schema[Str]   = Schema(tic.S())

  def arr[A](sa: Schema[A]): Schema[Arr[A]] =
    Schema(tic.Array(sa))

  def objectEmpty: Schema[Obj[{}]] =
    Schema(tic.Object.Empty())

  def objectSnoc[Init, PropName <: String, PropType](
    init: Schema[Obj[Init]],
    pname: SingletonValue[PropName],
    ptype: Schema[PropType],
  ): Schema[Obj[Init || PropName :: PropType]] =
    Schema(tic.Object.Snoc(init, pname, ptype))

  def unknown[S <: String](reason: SingletonValue[S]): Schema[Oops[S]] =
    Schema(tic.Unknown(reason))

  def unknown(reason: String): Schema[Oops[reason.type]] =
    Schema(tic.unknown(reason))
}
