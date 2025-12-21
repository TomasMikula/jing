package jing.openapi

import jing.openapi.model.*
import scala.quoted.{Quotes, Type}

/** Witnesses that `F[A]` carries enough information to synthesize `Type[A]`. */
trait AutoTyping[F[_]] {
  def typeOf[A](fa: F[A])(using Quotes): Type[A]

  extension [A](fa: F[A])
    def getType(using Quotes): Type[A] =
      typeOf(fa)
}
