package jing.openapi

import libretto.lambda.util.Applicative

private[openapi] opaque type Reader[R, A] =
  R => A

private[openapi] object Reader {
  def apply[R, A](f: R => A): Reader[R, A] =
    f

  def pure[R, A](a: A): Reader[R, A] =
    _ => a

  extension [R, A](fa: Reader[R, A]) {
    def run(env: R): A =
      fa(env)
  }

  private[openapi] given applicativeReader[R]: Applicative[Reader[R, _]] with {
    override def pure[A](a: A): Reader[R, A] =
      _ => a

    override def map[A, B](
      fa: Reader[R, A],
      f: A => B,
    ): Reader[R, B] =
      r => f(fa(r))

    override def zip[A, B](
      fa: Reader[R, A],
      fb: Reader[R, B],
    ): Reader[R, (A, B)] =
      r => (fa(r), fb(r))
  }
}
