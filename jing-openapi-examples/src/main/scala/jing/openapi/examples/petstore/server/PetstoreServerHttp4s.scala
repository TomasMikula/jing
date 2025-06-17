package jing.openapi.examples.petstore.server

import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.*
import jing.openapi.examples.petstore.api
import jing.openapi.model.Value.discriminatedUnion
import jing.openapi.server.http4s.{Http4sServerBuilder, Response, Routes}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.Status

object PetstoreServerHttp4s extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for
      store <- InMemoryPetstore.initialize
      httpApp = routes(store).http4sApp
      _ <-
        EmberServerBuilder
          .default[IO]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(httpApp)
          .build
          .use: server =>
            IO.println(s"Listening on ${server.address}") *>
            IO.never
    yield
      ExitCode.Success

  val serverBuilder =
    api
      .endpointList
      .interpret(using Http4sServerBuilder.forIO)

  def routes(store: InMemoryPetstore): Routes[IO] =
    serverBuilder
      .handleNext["/pet_POST"] { in =>
        val body = in.props.body
        val pet = body.discriminator match
          case "application/json"                  => body.assertCase["application/json"]
          case "application/xml"                   => body.assertCase["application/xml"]
          case "application/x-www-form-urlencoded" => body.assertCase["application/x-www-form-urlencoded"]
        store
          .createPet(pet)
          .map: pet =>
            Response:
              discriminatedUnion:
                _.`200`:
                  discriminatedUnion:
                    _.`application/json`:
                      pet
      }
      .handleNext["/pet_PUT"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/pet/findByStatus_GET"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/pet/findByTags_GET"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/pet/{petId}_GET"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/pet/{petId}_POST"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/pet/{petId}_DELETE"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/pet/{petId}/uploadImage_POST"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/store/inventory_GET"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/store/order_POST"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/store/order/{orderId}_GET"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/store/order/{orderId}_DELETE"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/user_POST"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/user/createWithList_POST"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/user/login_GET"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/user/logout_GET"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/user/{username}_GET"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/user/{username}_PUT"](_ => IO(Response.plainText(Status.NotImplemented)))
      .handleNext["/user/{username}_DELETE"](_ => IO(Response.plainText(Status.NotImplemented)))
      .end

  def routes_alternative_endpointNamesAsValues: Routes[IO] =
    serverBuilder
      .handle("/pet_POST")(???)
      .handle("/pet_PUT")(???)
      .handle("/pet/findByStatus_GET")(???)
      .handle("/pet/findByTags_GET")(???)
      .handle("/pet/{petId}_GET")(???)
      .handle("/pet/{petId}_POST")(???)
      .handle("/pet/{petId}_DELETE")(???)
      .handle("/pet/{petId}/uploadImage_POST")(???)
      .handle("/store/inventory_GET")(???)
      .handle("/store/order_POST")(???)
      .handle("/store/order/{orderId}_GET")(???)
      .handle("/store/order/{orderId}_DELETE")(???)
      .handle("/user_POST")(???)
      .handle("/user/createWithList_POST")(???)
      .handle("/user/login_GET")(???)
      .handle("/user/logout_GET")(???)
      .handle("/user/{username}_GET")(???)
      .handle("/user/{username}_PUT")(???)
      .handle("/user/{username}_DELETE")(???)
      .end

  def routes_alternative_direct: Routes[IO] =
    serverBuilder
      .next["/pet_POST"](???)
      .next["/pet_PUT"](???)
      .next["/pet/findByStatus_GET"](???)
      .next["/pet/findByTags_GET"](???)
      .next["/pet/{petId}_GET"](???)
      .next["/pet/{petId}_POST"](???)
      .next["/pet/{petId}_DELETE"](???)
      .next["/pet/{petId}/uploadImage_POST"](???)
      .next["/store/inventory_GET"](???)
      .next["/store/order_POST"](???)
      .next["/store/order/{orderId}_GET"](???)
      .next["/store/order/{orderId}_DELETE"](???)
      .next["/user_POST"](???)
      .next["/user/createWithList_POST"](???)
      .next["/user/login_GET"](???)
      .next["/user/logout_GET"](???)
      .next["/user/{username}_GET"](???)
      .next["/user/{username}_PUT"](???)
      .next["/user/{username}_DELETE"](???)
      .end


  def routes_alternative_directEndpointNamesAsValues: Routes[IO] =
    serverBuilder
      .on("/pet_POST")(???)
      .on("/pet_PUT")(???)
      .on("/pet/findByStatus_GET")(???)
      .on("/pet/findByTags_GET")(???)
      .on("/pet/{petId}_GET")(???)
      .on("/pet/{petId}_POST")(???)
      .on("/pet/{petId}_DELETE")(???)
      .on("/pet/{petId}/uploadImage_POST")(???)
      .on("/store/inventory_GET")(???)
      .on("/store/order_POST")(???)
      .on("/store/order/{orderId}_GET")(???)
      .on("/store/order/{orderId}_DELETE")(???)
      .on("/user_POST")(???)
      .on("/user/createWithList_POST")(???)
      .on("/user/login_GET")(???)
      .on("/user/logout_GET")(???)
      .on("/user/{username}_GET")(???)
      .on("/user/{username}_PUT")(???)
      .on("/user/{username}_DELETE")(???)
      .end

  def routes_alternative_namedTuple: Routes[IO] =
    serverBuilder.withRequestHandlersTuple((
      `/pet_POST` = ???,
      `/pet_PUT` = ???,
      `/pet/findByStatus_GET` = ???,
      `/pet/findByTags_GET` = ???,
      `/pet/{petId}_GET` = ???,
      `/pet/{petId}_POST` = ???,
      `/pet/{petId}_DELETE` = ???,
      `/pet/{petId}/uploadImage_POST` = ???,
      `/store/inventory_GET` = ???,
      `/store/order_POST` = ???,
      `/store/order/{orderId}_GET` = ???,
      `/store/order/{orderId}_DELETE` = ???,
      `/user_POST` = ???,
      `/user/createWithList_POST` = ???,
      `/user/login_GET` = ???,
      `/user/logout_GET` = ???,
      `/user/{username}_GET` = ???,
      `/user/{username}_PUT` = ???,
      `/user/{username}_DELETE` = ???,
    ))

  def routes_alternative_nAryFunction: Routes[IO] =
    serverBuilder.withRequestHandlers(
      `/pet_POST` = ???,
      `/pet_PUT` = ???,
      `/pet/findByStatus_GET` = ???,
      `/pet/findByTags_GET` = ???,
      `/pet/{petId}_GET` = ???,
      `/pet/{petId}_POST` = ???,
      `/pet/{petId}_DELETE` = ???,
      `/pet/{petId}/uploadImage_POST` = ???,
      `/store/inventory_GET` = ???,
      `/store/order_POST` = ???,
      `/store/order/{orderId}_GET` = ???,
      `/store/order/{orderId}_DELETE` = ???,
      `/user_POST` = ???,
      `/user/createWithList_POST` = ???,
      `/user/login_GET` = ???,
      `/user/logout_GET` = ???,
      `/user/{username}_GET` = ???,
      `/user/{username}_PUT` = ???,
      `/user/{username}_DELETE` = ???,
    )

  def routes_alternative_nAryMethod: Routes[IO] =
    serverBuilder.implementRequestHandlers(
      `/pet_POST` = ???,
      `/pet_PUT` = ???,
      `/pet/findByStatus_GET` = ???,
      `/pet/findByTags_GET` = ???,
      `/pet/{petId}_GET` = ???,
      `/pet/{petId}_POST` = ???,
      `/pet/{petId}_DELETE` = ???,
      `/pet/{petId}/uploadImage_POST` = ???,
      `/store/inventory_GET` = ???,
      `/store/order_POST` = ???,
      `/store/order/{orderId}_GET` = ???,
      `/store/order/{orderId}_DELETE` = ???,
      `/user_POST` = ???,
      `/user/createWithList_POST` = ???,
      `/user/login_GET` = ???,
      `/user/logout_GET` = ???,
      `/user/{username}_GET` = ???,
      `/user/{username}_PUT` = ???,
      `/user/{username}_DELETE` = ???,
    )
}
