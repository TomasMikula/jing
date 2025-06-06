package jing.macroUtil

import libretto.lambda.util.{Applicative, TypeEqK}
import scala.quoted.*

sealed trait Mode[Q <: Quotes, M] {
  val q: Q
  type InTerm
  type OutEff[A]

  def isTermSynth: OutEff[M =:= "term-synth"]

  given applicativeOutEff: Applicative[OutEff]

  def inTermProper(using M =:= "term-synth"): InTerm =:= q.reflect.Term
  def inTermRefOnly(using M =:= "type-synth"): InTerm =:= q.reflect.TermRef

  def outEffId(using M =:= "term-synth"): TypeEqK[OutEff, [x] =>> x]
  def outEffConstUnit(using M =:= "type-synth"): TypeEqK[OutEff, [x] =>> Unit]

  def term(t: InTerm): OutEff[q.reflect.Term]

  def name: M & String =
    this match
      case _: Mode.TypeSynth[q] => "type-synth"
      case _: Mode.TermSynth[q] => "term-synth"

  override def toString(): String =
    name
}

object Mode {
  class TypeSynth[Q <: Quotes & Singleton](using val q: Q) extends Mode[Q, "type-synth"] {
    override type InTerm = qr.TermRef
    override type OutEff[A] = Unit

    override def inTermProper(using impossible: "type-synth" =:= "term-synth"): qr.TermRef =:= qr.Term =
      throw AssertionError("""Impossible: "type-synth" =:= "term-synth"""")

    override def inTermRefOnly(using "type-synth" =:= "type-synth"): qr.TermRef =:= qr.TermRef =
      summon

    override def outEffId(using impossible: "type-synth" =:= "term-synth"): TypeEqK[[A] =>> Unit, [x] =>> x] =
      throw AssertionError("""Impossible: "type-synth" =:= "term-synth"""")

    override def outEffConstUnit(using "type-synth" =:= "type-synth"): TypeEqK[[A] =>> Unit, [x] =>> Unit] =
      TypeEqK.refl

    override def term(t: qr.TermRef): OutEff[qr.Term] =
      ()

    override def isTermSynth: Unit =
      ()

    override given applicativeOutEff: Applicative[[A] =>> Unit] with {
      override def pure[A](a: A): Unit = ()

      extension [A](fa: Unit) {
        override def map[B](f: A => B): Unit = ()
        override def zip[B](fb: Unit): Unit = ()
      }
    }
  }

  class TermSynth[Q <: Quotes & Singleton](using val q: Q) extends Mode[Q, "term-synth"] {
    override type InTerm = qr.Term
    override type OutEff[A] = A

    override def inTermProper(using "term-synth" =:= "term-synth"): qr.Term =:= qr.Term =
      summon

    override def inTermRefOnly(using impossible: "term-synth" =:= "type-synth"): qr.Term =:= qr.TermRef =
      throw AssertionError("""Impossible: "term-synth" =:= "type-synth"""")

    override def outEffId(using "term-synth" =:= "term-synth"): TypeEqK[[A] =>> A, [x] =>> x] =
      TypeEqK.refl

    override def outEffConstUnit(using impossible: "term-synth" =:= "type-synth"): TypeEqK[[A] =>> A, [x] =>> Unit] =
      throw AssertionError("""Impossible: "term-synth" =:= "type-synth"""")

    override def term(t: qr.Term): OutEff[qr.Term] =
      t

    override def isTermSynth: "term-synth" =:= "term-synth" =
      summon

    override given applicativeOutEff: Applicative[[A] =>> A] =
      Applicative.applicativeId
  }

  given typeSynth[Q <: Quotes & Singleton](using q: Q): TypeSynth[q.type] =
    TypeSynth[q.type]

  given termSynth[Q <: Quotes & Singleton](using q: Q): TermSynth[q.type] =
    TermSynth[q.type]

  def sameInTerm(using q: Quotes)[M](
    m1: Mode[q.type, M],
    m2: Mode[q.type, M],
  ): m1.InTerm =:= m2.InTerm =
    m1 match
      case _: TermSynth[q] => m1.inTermProper andThen m2.inTermProper.flip
      case _: TypeSynth[q] => m1.inTermRefOnly andThen m2.inTermRefOnly.flip

  def sameOutEff[Q <: Quotes, M](
    m1: Mode[Q, M],
    m2: Mode[Q, M],
  ): TypeEqK[m1.OutEff, m2.OutEff] =
    m1 match
      case _: TermSynth[q] => m1.outEffId andThen m2.outEffId.flip
      case _: TypeSynth[q] => m1.outEffConstUnit andThen m2.outEffConstUnit.flip

  infix enum IsSubsumedBy[M, N] {
    case TypeSynthSubsumedByTermSynth extends ("type-synth" IsSubsumedBy "term-synth")
    case TermSynthRefl extends ("term-synth" IsSubsumedBy "term-synth")
    case TypeSynthRefl extends ("type-synth" IsSubsumedBy "type-synth")

    def downgrader(using q: Quotes, m: Mode[q.type, M], n: Mode[q.type, N]): [A] => n.OutEff[A] => m.OutEff[A] =
      this match
        case TypeSynthSubsumedByTermSynth => [A] => a => m.outEffConstUnit.flip.at(())
        case TermSynthRefl => [A] => a => Mode.sameOutEff(n, m).at(a)
        case TypeSynthRefl => [A] => a => Mode.sameOutEff(n, m).at(a)

  }

  object IsSubsumedBy {
    given termSynthRefl: ("term-synth" IsSubsumedBy "term-synth") = TermSynthRefl
    given typeSynthSubsumedByAnything[Q <: Quotes, N](using n: Mode[Q, N]): ("type-synth" IsSubsumedBy N) =
      n match
        case _: TypeSynth[n] => TypeSynthRefl
        case _: TermSynth[n] => TypeSynthSubsumedByTermSynth
  }
}
