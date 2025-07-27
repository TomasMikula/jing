package jing.openapi.client.default

import jing.openapi.model.Value.Lenient.Oopsy
import libretto.lambda.util.NonEmptyList

import java.io.IOException
import java.io.StringWriter

sealed trait Result[T] {
  import Result.*

  def flatMap[U](f: T => Result[U]): Result[U]

  def map[U](f: T => U): Result[U] =
    flatMap(t => Result.Succeeded(f(t)))

  /** Returns the success value.
   *
   * @throws IllegalStateException if this `Result` is not [[Succeeded]].
   */
  def assertSuccess(failureMsg: String): T =
    this match
      case Succeeded(value) =>
        value
      case Failed(failure) =>
        throw IllegalStateException(s"$failureMsg: $failure")

  /** Returns the failure.
   *
   * @throws IllegalStateException if this `Result` is not [[Failed]].
   */
  def assertFailure: Failure =
    this match
      case Succeeded(value) =>
        throw IllegalStateException(s"Expected failure, but succeeded with $value")
      case Failed(failure) =>
        failure

}

object Result {
  case class Succeeded[T](value: T) extends Result[T] {
    override def flatMap[U](f: T => Result[U]): Result[U] =
      f(value)
  }

  case class Failed[T](failure: Failure) extends Result[T] {
    override def flatMap[U](f: T => Result[U]): Result[U] =
      Failed(failure)
  }

  sealed trait Failure

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
    case class UnsupportedSchema(problems: NonEmptyList[Oopsy[? <: String]]) extends NotSupported

    case class UnexpectedError(thrown: Throwable) extends Failure {
      override def toString(): String =
        val builder = new StringWriter()
        builder.append("Unexpected error: ")
        builder.append(thrown.getMessage())
        thrown.printStackTrace(new java.io.PrintWriter(builder))
        builder.toString()
    }
  }

  def ioError[T](thrown: IOException): Result[T] = Failed(Failure.IOError(thrown))
  def serverError[T](code: Int, body: String): Result[T] = Failed(Failure.ServerError(code, body))
  def unexpectedStatusCode[T](code: Int, body: String): Result[T] = Failed(Failure.UnexpectedStatusCode(code, body))
  def missingContentTypeHeader[T](statusCode: Int, body: String): Result[T] = Failed(Failure.MissingContentTypeHeader(statusCode, body))
  def unexpectedContentType[T](statusCode: Int, contentType: String, body: String): Result[T] = Failed(Failure.UnexpectedContentType(statusCode, contentType, body))
  def parseError[T](statusCode: Int, message: String, underlying: Throwable): Result[T] = Failed(Failure.ParseError(statusCode, message, underlying))
  def schemaViolation[T](details: String): Result[T] = Failed(Failure.SchemaViolation(details))
  def unsupportedContentType[T](statusCode: Int, contentType: String, body: String): Result[T] = Failed(Failure.UnsupportedContentType(statusCode, contentType, body))
  def unsupportedSchema[T](problems: NonEmptyList[Oopsy[? <: String]]): Result[T] = Failed(Failure.UnsupportedSchema(problems))
  def unexpectedError[T](thrown: Throwable): Result[T] = Failed(Failure.UnexpectedError(thrown))

  // TODO: accumulate errors (but first need to be able to represent multiple errors)
  def map2[A, B, R](ra: Result[A], rb: Result[B])(f: (A, B) => R): Result[R] =
    ra match
      case Succeeded(a) =>
        rb match
          case Succeeded(b) => Succeeded(f(a, b))
          case Failed(e) => Failed(e)
      case Failed(e) => Failed(e)
}
