# JING: Just Import 'N' Go

## Goal

 **Frictionless** _spec-first_ programming against OpenAPI

### Sources of Friction

 - **Setting up code generation**

   State of the art in specification-first programming relies on first generating some code (data types, interfaces, stubs, ...) from the specification.
   This usually means configuring a build step to run prior to compilation,
   potentially with the help of a dedicated build plugin for your build tool of choice.

 - **Dealing with low-level details**

   ... like serialization or HTTP protocol.

These present substantial accidental coplexity, especially for the inital, explorative phase of a project.

## Solution

Just Import 'N' Go 😎

Use compile-time metaprogramming to generate code from spec "on-the-fly" in the usual compile-time, without a previous codegen step.
This approach requires **no build setup** other than adding `jing` to library dependencies.

Additionally, the generated endpoints are **ready to use,** using reasonable default choices of HTTP and JSON libraries behind the scenes.

## Example

```scala
val api = jing.openapi("https://petstore3.swagger.io/api/v3/openapi.json")

api
  .paths
  .`/pet/findByStatus`
  .Get
  .interpret(using DefaultClient)
  .params(_
    .set("status", "available")
  )
  .runAgainst("https://petstore3.swagger.io/api/v3")
```

See more in [TestApp.scala](https://github.com/TomasMikula/jing/blob/main/jing-openapi-examples/src/main/scala/jing/openapi/examples/TestApp.scala).

## Design Principles

 - **Discoverability.**

   Import an API and discover the rest from there, without prior knowledge of the API or deep knowledge of the `jing` library.

   👍 type-drivenness, IDE-friendliness, helpful compilation errors

   👎 chains of implicits, orphan typeclasses
   
 - **Safety**

   Make it very hard to shoot yourself in the foot.

   👍 type-safety, illegal states unrepresentable

 - **Simple things simple.**

   👍 shortcuts for common scenarios

 - **Complex things possible.**

   If needed, delegate to state-of-the-art 3rd party libraries, but provide a smooth handover of control.

   👎 low complexity ceiling

## Non-goals

 - _Full OpenAPI support._ OpenAPI is unnecessarily bloated. `jing` will aim to support a reasonable, computation sympathetic subset of OpenAPI.
   Feel free to open a ticket if you need something that's missing.

## Status

Successful proof of concept.

Lots of OpenAPI features are still missing.

There's no support for implementing servers yet (but is planned).

There's lot's of space for improvement even with regards to the above principles
(but feel free to call me out on violations thereof, at least I will know that you care).
