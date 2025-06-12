package jing.openapi.examples.petstore.server

import jing.openapi.examples.petstore.api

import jing.openapi.server.http4s.Http4sServerBuilder

object PetstoreServerHttp4s extends App {

  val serverBuilder =
    api
      .endpointList
      .interpret(using Http4sServerBuilder.forIO)

  serverBuilder
    .handleNext["/pet_POST"](???)
    .handleNext["/pet_PUT"](???)
    .handleNext["/pet/findByStatus_GET"](???)
    .handleNext["/pet/findByTags_GET"](???)
    .handleNext["/pet/{petId}_GET"](???)
    .handleNext["/pet/{petId}_POST"](???)
    .handleNext["/pet/{petId}_DELETE"](???)
    .handleNext["/pet/{petId}/uploadImage_POST"](???)
    .handleNext["/store/inventory_GET"](???)
    .handleNext["/store/order_POST"](???)
    .handleNext["/store/order/{orderId}_GET"](???)
    .handleNext["/store/order/{orderId}_DELETE"](???)
    .handleNext["/user_POST"](???)
    .handleNext["/user/createWithList_POST"](???)
    .handleNext["/user/login_GET"](???)
    .handleNext["/user/logout_GET"](???)
    .handleNext["/user/{username}_GET"](???)
    .handleNext["/user/{username}_PUT"](???)
    .handleNext["/user/{username}_DELETE"](???)
    .end

  def alternative_endpointNamesAsValues =
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

  def alternative_direct =
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


  def alternative_directEndpointNamesAsValues =
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

  def alternative_namedTuple =
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

  def alternative_nAryFunction =
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

  def alternative_nAryMethod =
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
