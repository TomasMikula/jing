package jing.openapi

private[openapi] opaque type Unrelated[A, B] = Unit

private[openapi] object Unrelated {
  def apply[A, B](): Unrelated[A, B] =
    ()

  given Substitutive[Unrelated] with {
    override def refl[A]: Unrelated[A, A] = ()
    override def trans[A, B, C](r: Unrelated[A, B], s: Unrelated[B, C]): Unrelated[A, C] = ()
    override def subst[F[_], A, B](rel: Unrelated[A, B]): Unrelated[F[A], F[B]] = ()
  }
}
