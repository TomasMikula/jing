package jing.openapi

import jing.openapi.model.{||, Obj, ScalaValueOf, Schema, SchemaMotif, Str}
import libretto.lambda.util.{Exists, SingletonType}
import scala.annotation.tailrec

/** Schema with unresolved references to other schemas. */
private[openapi] enum ProtoSchema {
  case Proper(value: SchemaMotif[[A] =>> ProtoSchema, ?])
  case Ref(schemaName: String)
  case Unsupported(details: String)

  def resolve(
    alreadyResolved: Map[String, Schema.Labeled[String, ?]],
  ): Schema.Labeled[String, ?] =
    this match
      case Proper(value) =>
        Schema.Labeled.Unlabeled:
          value.wipeTranslate[Schema.Labeled[String, _]]([X] => ps => Exists(ps.resolve(alreadyResolved)))
      case Ref(schemaName) =>
        alreadyResolved.get(schemaName) match
          case Some(schema) =>
            Schema.Labeled.WithLabel(schemaName, schema)
          case None =>
            val msg = s"Unresolved schema $schemaName"
            Schema.Labeled.Unsupported(SingletonType(msg))
      case Unsupported(details) =>
          Schema.Labeled.Unsupported(SingletonType(details))
}

private[openapi] object ProtoSchema {
  import jing.openapi.model.{SchemaMotif as motif}

  def i32: ProtoSchema = Proper(motif.I32())
  def i64: ProtoSchema = Proper(motif.I64())
  def str: ProtoSchema = Proper(motif.S())
  def bool: ProtoSchema = Proper(motif.B())

  def strEnum(value: String, values: String*): ProtoSchema =
    Proper(SchemaMotif.Enumeration.str(value, values*))

  def int32Enum(value: Int, values: Int*): ProtoSchema =
    Proper(SchemaMotif.Enumeration.int32(value, values*))

  def int64Enum(value: Long, values: Long*): ProtoSchema =
    Proper(SchemaMotif.Enumeration.int64(value, values*))

  def boolEnum(value: Boolean, values: Boolean*): ProtoSchema =
    Proper(SchemaMotif.Enumeration.bool(value, values*))

  def arr(elemSchema: ProtoSchema): ProtoSchema =
    Proper(motif.Array(elemSchema))

  def obj(props: List[(String, Boolean, ProtoSchema)]): ProtoSchema = {
    @tailrec
    def go(
      acc: motif.Object[[A] =>> ProtoSchema, ?],
      remaining: List[(String, Boolean, ProtoSchema)],
    ): motif.Object[[A] =>> ProtoSchema, ?] =
      remaining match
        case Nil =>
          acc
        case (n, isRequired, s) :: ps =>
          if isRequired
          then go(motif.Object.snoc(acc, n, s), ps)
          else go(motif.Object.snocOpt(acc, n, s), ps)

    Proper(go(motif.Object.empty, props))
  }

  /** Schema with unresolved references categorized as backward or forward,
   *  resulting from topological sort (where presence of forward references
   *  indicates cyclic references).
   */
  enum Oriented {
    case Proper(value: SchemaMotif[[A] =>> ProtoSchema.Oriented, ?])
    case BackwardRef(schemaName: String)
    case ForwardRef(schemaName: String, cycle: List[String])
    case UnresolvedRef(schemaName: String)
    case Unsupported(details: String)
  }

  object Oriented {
    def sortTopologically(schemas: List[(String, ProtoSchema)]): List[(String, ProtoSchema.Oriented)] = {
      def go1(
        dependents: List[String],
        acc: List[(String, Oriented)],
        subject: ProtoSchema,
        remaining: List[(String, ProtoSchema)]
      ): (
        List[(String, Oriented)],   // extended acc
        Oriented,                   // oriented subject
        List[(String, ProtoSchema)] // remaining
      ) =
        subject match {
          case ProtoSchema.Proper(s) =>
            val ((acc1, remaining1), res) =
            s.wipeTranslateA[
              ReaderState[List[String], (List[(String, Oriented)], List[(String, ProtoSchema)]), _],
              [A] =>> Oriented,
            ](
              [X] => (protoSchema) => ReaderState { case (dependents, (acc, remaining)) =>
                val (acc1, res, remaining1) = go1(dependents, acc, protoSchema, remaining)
                ((acc1, remaining1), Exists(res))
              }
            ).run(dependents, (acc, remaining))
            (acc1, Oriented.Proper(res), remaining1)
          case ProtoSchema.Ref(schemaName) =>
            if (acc.exists { case (n, _) => n == schemaName })
              (acc, Oriented.BackwardRef(schemaName), remaining)
            else
              val i = dependents.indexOf(schemaName)
              if (i >= 0) {
                val cycle = dependents.head :: dependents.take(i+1).reverse
                (acc, Oriented.ForwardRef(schemaName, cycle), remaining)
              } else {
                remaining.find { case (n, _) => n == schemaName } match
                  case None =>
                    (acc, Oriented.UnresolvedRef(schemaName), remaining)
                  case Some((_, subject1)) =>
                    val remaining1 = remaining.filter { case (n, _) => n != schemaName }
                    val (acc2, oriented2, remaining2) = go1(schemaName :: dependents, acc, subject1, remaining1)
                    (acc2 :+ (schemaName, oriented2), Oriented.BackwardRef(schemaName), remaining2)
              }
          case ProtoSchema.Unsupported(details) =>
            (acc, Oriented.Unsupported(details), remaining)
        }

      @tailrec
      def go(
        acc: List[(String, Oriented)],
        remaining: List[(String, ProtoSchema)]
      ): List[(String, Oriented)] =
        remaining match {
          case Nil =>
            acc
          case (name, schema) :: tail =>
            val (acc1, oriented, remaining1) =
              go1(dependents = List(name), acc = acc, subject = schema, remaining = tail)
            go(acc1 :+ (name, oriented), remaining1)
        }

      go(acc = Nil, remaining = schemas)
    }

    def resolveAcyclic(schemas: List[(String, ProtoSchema.Oriented)]): List[(String, Schema.Labeled[String, ?])] = {
      @tailrec
      def go(
        revAcc: List[(String, Schema.Labeled[String, ?])],
        accMap: Map[String, Schema.Labeled[String, ?]], // contains the same elements as revAcc
        remaining: List[(String, ProtoSchema.Oriented)],
      ): List[(String, Schema.Labeled[String, ?])] =
        remaining match
          case (name, schema) :: tail =>
            val resolved = resolve(schema, accMap)
            go((name, resolved) :: revAcc, accMap.updated(name, resolved), tail)
          case Nil =>
            revAcc.reverse

      go(Nil, Map.empty, schemas)
    }

    private def resolve(
      schema: ProtoSchema.Oriented,
      alreadyResolved: Map[String, Schema.Labeled[String, ?]],
    ): Schema.Labeled[String, ?] =
      schema match
        case Proper(value) =>
          Schema.Labeled.Unlabeled:
            value.wipeTranslate[Schema.Labeled[String, _]]([X] => pso => Exists(resolve(pso, alreadyResolved)))
        case BackwardRef(schemaName) =>
          alreadyResolved.get(schemaName) match
            case Some(schema) =>
              Schema.Labeled.WithLabel(schemaName, schema)
            case None =>
              throw new NoSuchElementException(s"Schema '$schemaName' not found among previously resolved schemas. This is a bug in JING, as it was previously determined to be a backward reference.")
        case ForwardRef(schemaName, cycle) =>
          val msg = s"Unsupported recursive schema: ${cycle.mkString(" -> ")}"
          Schema.Labeled.Unsupported(SingletonType(msg))
        case UnresolvedRef(schemaName) =>
          val msg = s"Unresolved schema $schemaName"
          Schema.Labeled.Unsupported(SingletonType(msg))
        case Unsupported(details) =>
          Schema.Labeled.Unsupported(SingletonType(details))
  }

}
