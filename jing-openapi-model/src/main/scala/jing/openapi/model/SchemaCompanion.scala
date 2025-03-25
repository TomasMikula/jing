package jing.openapi.model

trait SchemaCompanion[A, B] {
  val schema: Schema[A]
  def apply(b: Value[B]): Value[A]
  def unapply(a: Value[A]): Some[Value[B]]
}
