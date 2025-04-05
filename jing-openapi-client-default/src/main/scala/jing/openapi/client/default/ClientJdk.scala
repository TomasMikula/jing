package jing.openapi.client.default

import java.net.http.{HttpClient, HttpRequest}
import java.net.http.HttpResponse.BodyHandlers

import jing.openapi.model.{Client, Value}
import jing.openapi.model.HttpThunk

class ClientJdk extends Client {

  // TODO: a proper result type
  override type Result[T] = String

  private val client =
    // TODO: manage as a resource, i.e. needs to be released
    HttpClient.newBuilder().build()

  override def runRequest[O](
    baseUrl: String,
    req: HttpThunk[O],
  ): Result[O] = {
    val HttpThunk.Impl(path, input, respSchema) = req

    val queryParams =
      input
        .queryParams
        .map { _.iterator.map { case (k, v) => s"${urlEncode(k)}=${urlEncode(v)}" }.mkString("?", "&", "") }
        .getOrElse("")

    val uri = new java.net.URI(baseUrl + path + queryParams)

    client
      .send(
        HttpRequest.newBuilder(uri).build(), // TODO: body
        BodyHandlers.ofString(), // XXX
      )
      .body()
  }

  private def urlEncode(s: String): String =
    s // TODO

  private def urlEncode[T](v: Value[T]): String =
    urlEncode(stringify(v))

  private def stringify[T](v: Value[T]): String =
    import Value.*
    v match
        case StringValue(s) => s
        case Int64Value(i) => i.toString
        case o: Object[props] =>
          // TODO: objects in query parameters should either
          //  - be disallowed by construction; or
          //  - have an associated encoder
          Value.toMap(o)
            .view.mapValues(stringify(_))
            .iterator
            .map { case (k, v) => s"$k:$v" }
            .mkString("{", ",", "}")

}
