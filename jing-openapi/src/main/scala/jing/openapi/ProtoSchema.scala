package jing.openapi

import jing.openapi.model.Schematic

/** Schema with unresolved references to other schemas. */
private[openapi] enum ProtoSchema {
  case Proper(value: Schematic[[A] =>> ProtoSchema, ?])
  case Ref(schemaName: String)
  case Unsupported(details: String)
}

object ProtoSchema {
  import jing.openapi.model.{Schematic as tic}

  def i64: ProtoSchema = Proper(tic.I64())
  def str: ProtoSchema = Proper(tic.S())

  def arr(elemSchema: ProtoSchema): ProtoSchema =
    Proper(tic.Array(elemSchema))
}
