package jing.openapi.model

trait TotalExtractor[A, B] {
  def unapply(a: A): Some[B]
}
