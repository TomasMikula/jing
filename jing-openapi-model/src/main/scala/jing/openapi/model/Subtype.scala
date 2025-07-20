package jing.openapi.model

/** Reified subtype relation. Like `<:<`, but pattern-matchable.
 * Pattern matching on it brings the subtype relation back into compiler's attention.
 */
private[model] enum Subtype[-A, +B]:
  case Refl[T]() extends Subtype[T, T]

private[model] object Subtype:
  def apply[A, B](ev: A <:< B): Subtype[A, B] =
    ev.substituteCo[Subtype[A, _]]:
      Subtype.Refl[A]()
