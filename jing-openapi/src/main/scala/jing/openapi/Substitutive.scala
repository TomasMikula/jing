package jing.openapi

private[openapi] trait Substitutive[Rel[_, _]] {
  def refl[A]: Rel[A, A]
  def trans[A, B, C](r: Rel[A, B], s: Rel[B, C]): Rel[A, C]
  def subst[F[_], A, B](rel: Rel[A, B]): Rel[F[A], F[B]]

  extension [A, B](rel: Rel[A, B]) {
    def lift[F[_]]: Rel[F[A], F[B]] =
      subst(rel)

    infix def andThen[C](s: Rel[B, C]): Rel[A, C] =
      trans(rel, s)
  }

  def biLift[A, B, X, Y](
    r: Rel[A, X],
    s: Rel[B, Y],
  )[F[_, _]]: Rel[F[A, B], F[X, Y]] =
    r.lift[[t] =>> F[t, B]] andThen s.lift[[t] =>> F[X, t]]

}

private[openapi] object Substitutive {
  given Substitutive[=:=] with {
    override def refl[A]: A =:= A =
      summon

    override def trans[A, B, C](r: A =:= B, s: B =:= C): A =:= C =
      r andThen s

    override def subst[F[_], A, B](rel: A =:= B): F[A] =:= F[B] =
      rel.liftCo[F]
  }
}
