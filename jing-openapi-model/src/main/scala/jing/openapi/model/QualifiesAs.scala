package jing.openapi.model

/** Witnesses that a values of the Scala type `S` qualifies for domain type `D`. */
infix enum QualifiesAs[-S, D] {
  case S extends (String QualifiesAs Str)
  case I extends (Int QualifiesAs Int32)
  case L extends (Long QualifiesAs Int64)
  case B extends (Boolean QualifiesAs Bool)
}

object QualifiesAs {
  given (String QualifiesAs Str) = S
  given (Int QualifiesAs Int32) = I
  given (Long QualifiesAs Int64) = L
  given (Boolean QualifiesAs Bool) = B
}
