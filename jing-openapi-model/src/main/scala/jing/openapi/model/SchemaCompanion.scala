package jing.openapi.model

import scala.NamedTuple.NamedTuple

class SchemaCompanion[A, B](
  val schema: Schema[A]
)(using
  protected val evidence: A =:= B
) {
  def from(b: Value[B]): Value[A] =
    evidence.substituteContra(b)

  def deconstruct(a: Value[A]): Value[B] =
    evidence.substituteCo(a)
}

// TFrom and TTo are supposed to be instantiated exactly to their upper and lower bound, respectively.
// TFrom and TTo might thus seem superfluous. However, named tuple reduction for IDE hints on `apply` and `unapply`
//  works better if the named tuple is assigned to an outer type parameter, than reducing it on the fly at method call site.
class ObjectSchemaCompanion[
  A,
  Props,
  TFrom <: NamedTuple[PropNamesTuple[Props], PropTypesTupleU[Value, Props]],
  TTo   >: NamedTuple[PropNamesTuple[Props], PropTypesTupleO[Value, Props]],
](
  schema: Schema[A],
)(using
  A =:= Obj[Props],
) extends SchemaCompanion[A, Obj[Props]](schema) {
  private given propertyList: PropertyList[Props] =
    evidence.substituteCo(schema).propertyList

  /** Constructs a Value from named tuple. */
  def apply(t: TFrom): Value[A] =
    evidence.substituteContra:
      Value.Obj.fromTuple[Props](t)

  /** Deconstructs the Value into a named tuple. */
  def toNamedTuple(a: Value[A]): NamedTuple[PropNamesTuple[Props], PropTypesTupleO[Value, Props]] =
    val obj = deconstruct(a)
    obj.toNamedTuple()

  /** Extractor that deconstructs the Value into a named tuple. */
  def unapply(a: Value[A]): Some[TTo] =
    Some(toNamedTuple(a))
}
