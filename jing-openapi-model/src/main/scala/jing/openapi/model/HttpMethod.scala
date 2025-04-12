package jing.openapi.model

enum HttpMethod {
  case Get
  case Post
  case Put
  case Delete
  case Head
  case Options
  case Patch
  case Trace

  def nameUpperCase: String =
    this.toString.toUpperCase
}
