package jing.openapi.model

abstract class OpenApiSpec {
  /** All schema definitions from OpenAPI spec's `components.schemas`. */
  val schemas: {}

  /** All path definitions from OpenAPI spec's `paths`. */
  val paths: {}

  /** A (heterogenous) list of all endpoints found in [[paths]]. */
  val endpointList: EndpointList[?, ?]
}
