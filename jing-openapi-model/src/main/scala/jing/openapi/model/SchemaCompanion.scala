package jing.openapi.model

import scala.NamedTuple.NamedTuple

class SchemaCompanion[A, B](
  val schema: Schema[A]
)(using
  protected val evidence: A =:= B
) {
  def apply(b: Value[B]): Value[A] =
    evidence.substituteContra(b)

  def unapply(a: Value[A]): Some[Value[B]] =
    Some(evidence.substituteCo(a))
}

// T is going to be instantiated to its exact upper bound, but named tuple reduction for IDE hints
// works better this way for the argument type of fromNamedTuple
class ObjectSchemaCompanion[A, Props, T <: NamedTuple[PropNamesTuple[Props], PropTypesTupleU[Value, Props]]](
  schema: Schema[A],
)(using
  A =:= Obj[Props],
) extends SchemaCompanion[A, Obj[Props]](schema) {
  given propertyList: PropertyList[Props] =
    evidence.substituteCo(schema).propertyList

  def fromNamedTuple(t: T): Value[A] =
    evidence.substituteContra:
      Value.Obj[Props](_(t))

  extension (a: Value[A])
    def toNamedTuple: NamedTuple[PropNamesTuple[Props], PropTypesTupleO[Value, Props]] =
      val Some(b) = unapply(a)
      b.toNamedTuple()
}
