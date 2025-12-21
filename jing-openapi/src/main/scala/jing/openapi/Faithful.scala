package jing.openapi

import jing.openapi.model.*
import jing.openapi.model.Schema.Labeled

trait Faithful[F[_]] {
  extension [Ps](a: F[Obj[Ps]])
    def motif: ObjectMotif[F, F, Ps]
}

object Faithful {
  given Faithful[Schema] {
    extension [Ps](s: Schema[Obj[Ps]])
      override def motif: ObjectMotif[Schema, Schema, Ps] =
        s match
          case Schema.Proper(motif) => motif.asObject.value
  }

  given [L] => Faithful[Schema.Labeled[L, _]] {
    extension [Ps](s: Schema.Labeled[L, Obj[Ps]])
      override def motif: ObjectMotif[Schema.Labeled[L, _], Schema.Labeled[L, _], Ps] =
        s match
          case Labeled.Unlabeled(motif) => motif.asObject.value
          case Labeled.WithLabel(label, schema) => schema.motif
  }
}
