package jing.openapi

private sealed trait Exists[F[_]] {
  type T
  def value: F[T]
}

private object Exists {
  case class Some[F[_], A](value: F[A]) extends Exists[F] {
    type T = A
  }
}
