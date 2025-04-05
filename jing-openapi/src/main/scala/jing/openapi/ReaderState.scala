package jing.openapi

import libretto.lambda.util.Applicative

private[openapi] opaque type ReaderState[R, S, A] =
  (R, S) => (S, A)

private[openapi] object ReaderState {
  def apply[R, S, A](f: (R, S) => (S, A)): ReaderState[R, S, A] =
    f

  extension [R, S, A](fa: ReaderState[R, S, A]) {
    def run(env: R, initialState: S): (S, A) =
      fa(env, initialState)
  }

  private[openapi] given applicativeReaderState[R, S]: Applicative[ReaderState[R, S, _]] with {
    override def pure[A](a: A): ReaderState[R, S, A] =
      (r, s) => (s, a)

    extension [A](fa: ReaderState[R, S, A]) {
      override def map[B](f: A => B): ReaderState[R, S, B] =
        (r, s) =>
          val (s1, a) = fa(r, s)
          (s1, f(a))

      override def zip[B](fb: ReaderState[R, S, B]): ReaderState[R, S, (A, B)] =
        (r, s) =>
          val (s1, a) = fa(r, s)
          val (s2, b) = fb(r, s1)
          (s2, (a, b))
    }
  }
}
