package jing.openapi.model

import libretto.lambda.Items1Named.Member
import libretto.lambda.util.SingletonType

infix opaque type IsCaseOf[Label, Cases] <: { type Type } =
  Member[||, ::, Label, ?, Cases]

object IsCaseOf {
  def fromMember[Label, A, Cases](
    m: Member[||, ::, Label, A, Cases],
  ): IsCaseOf[Label, Cases] { type Type = A } =
    m

  def toMember[Label, A, Cases](
    c: IsCaseOf[Label, Cases] { type Type = A },
  ): Member[||, ::, Label, A, Cases] =
    c.asInstanceOf[Member[||, ::, Label, A, Cases]] // XXX
    // Avoid asInstanceOf after libretto-lambda 0.3.5 is released:
    // c.typeWitness.substituteCo[Member[||, ::, Label, _, Cases]](c)

  given isSingleCaseOf[Label <: String, A](using
    label: SingletonType[Label],
  ): (IsCaseOf[Label, Label :: A] { type Type = A }) =
    Member.Single(label)

  given isLastCaseOf[Init, Label <: String, A](using
    label: SingletonType[Label],
  ): (IsCaseOf[Label, Init || Label :: A] { type Type = A }) =
    Member.InLast(label)

  given isCaseInInitOf[Label <: String, A, Init, LabelZ, Z](using
    init: (Label IsCaseOf Init) { type Type = A },
  ): (IsCaseOf[Label, Init || LabelZ :: Z] { type Type = A }) =
    Member.InInit(toMember(init))
}