package jing.openapi.model

import libretto.lambda.util.{SingletonType, TypeEq}
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.TypeEq.Refl

/** Witnesses that the domain type `T` represents a singleton string schema. */
sealed trait SingletonStringSchema[T] {
  import SingletonStringSchema.*

  def stringValue: String =
    this match
      case Constant(value) => value.value
      case SingletonEnum(value) => value.value

  def isFullyRefined[U](r: T IsRefinedBy U): T =:= U
}

object SingletonStringSchema {
  case class Constant[S <: String](value: SingletonType[S]) extends SingletonStringSchema[Const[S]] {
    override def isFullyRefined[U](r: Const[S] IsRefinedBy U): Const[S] =:= U =
      r.preservesConst match
        case ex @ Indeed((TypeEq(Refl()), sRt)) =>
          summon[U =:= Const[ex.T]]
          sRt.isStringRefl match
            case TypeEq(Refl()) =>
              summon[Const[S] =:= U]
  }

  case class SingletonEnum[S <: String](value: SingletonType[S]) extends SingletonStringSchema[Enum[Str, Void || S]] {
    override def isFullyRefined[U](r: Enum[Str, Void || S] IsRefinedBy U): Enum[Str, Void || S] =:= U =
      r.preservesEnum match
        case Indeed(Indeed((TypeEq(Refl()), r1, r2))) =>
          r1.isStrRefl match
            case TypeEq(Refl()) =>
              r2.preserves_|| match
                case Indeed(Indeed((TypeEq(Refl()), r3, r4))) =>
                  r3.isVoidRefl match
                    case TypeEq(Refl()) =>
                      r4.isStringRefl match
                        case TypeEq(Refl()) =>
                          summon[Enum[Str, Void || S] =:= U]
  }
}
