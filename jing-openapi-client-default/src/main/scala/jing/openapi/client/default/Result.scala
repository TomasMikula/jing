package jing.openapi.client.default

import java.io.IOException

sealed trait Result[+T] {
  def flatMap[U](f: T => Result[U]): Result[U]

  def map[U](f: T => U): Result[U] =
    flatMap(t => Result.Success(f(t)))
}

object Result {
  case class Success[T](value: T) extends Result[T] {
    override def flatMap[U](f: T => Result[U]): Result[U] =
      f(value)
  }

  sealed trait Failure extends Result[Nothing] {
    override def flatMap[U](f: Nothing => Result[U]): Result[U] =
      this
  }

  object Failure {
    case class IOError(thrown: IOException) extends Failure
    case class ServerError(code: Int, body: String) extends Failure

    sealed trait InvalidContent extends Failure
    case class UnexpectedStatusCode(code: Int, body: String) extends InvalidContent
    case class MissingContentTypeHeader(statusCode: Int, body: String) extends InvalidContent
    case class UnexpectedContentType(statusCode: Int, contentType: String, body: String) extends InvalidContent
    case class ParseError(statusCode: Int, message: String, underlying: Throwable) extends InvalidContent
    case class SchemaViolation(details: String) extends InvalidContent

    sealed trait NotSupported extends Failure
    case class UnsupportedContentType(statusCode: Int, contentType: String, body: String) extends NotSupported

    case class UnexpectedError(thrown: Throwable) extends Failure
  }

  // TODO: accumulate errors (but first need to be able to represent multiple errors)
  def map2[A, B, R](ra: Result[A], rb: Result[B])(f: (A, B) => R): Result[R] =
    ra match
      case Success(a) =>
        rb match
          case Success(b) => Success(f(a, b))
          case e: Failure => e
      case e: Failure => e
}
