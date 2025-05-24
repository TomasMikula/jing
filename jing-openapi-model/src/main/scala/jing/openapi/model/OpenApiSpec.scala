package jing.openapi.model

abstract class OpenApiSpec {
  val schemas: {}
  val paths: {}

  /** A (heterogenous) list of all endpoints found in [[paths]]. */
  val endpointList: EndpointList[?]
}
