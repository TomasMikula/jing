package jing.openapi

import jing.openapi.model.{||, Obj, ScalaValueOf, Schema, SchemaMotif, Str}
import libretto.lambda.util.{Applicative, Exists, SingletonType}
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

  import ProtoSchema.*

  private def resolveA[F[_]](using
    r: Resolver[ProtoSchema.Oriented, Option[Cycle], F],
    F: Applicative[F],
  ): F[ProtoSchema.Oriented] =
    this match
      case Proper(value) =>
        value
          .wipeTranslateA[F, [x] =>> ProtoSchema.Oriented]([X] => ps => ps.resolveA[F].map(Exists(_)))
          .map(Oriented.Proper(_))
      case Ref(schemaName) =>
        r.resolve(schemaName) map:
          case Right(schema) =>
            Oriented.BackwardRef(schemaName)
          case Left(None) =>
            Oriented.Unsupported(s"Unresolved schema $schemaName")
          case Left(Some(Cycle(cycle))) =>
            Oriented.ForwardRef(schemaName, cycle)
      case Unsupported(details) =>
        F.pure(Oriented.Unsupported(details))
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
  private enum Oriented {
    case Proper(value: SchemaMotif[[A] =>> ProtoSchema.Oriented, ?])
    case BackwardRef(schemaName: String)
    case ForwardRef(schemaName: String, cycle: List[String])
    case UnresolvedRef(schemaName: String)
    case Unsupported(details: String)

    private[ProtoSchema] def resolve(
      alreadyResolved: Map[String, Schema.Labeled[String, ?]],
    ): Schema.Labeled[String, ?] =
      this match
        case Oriented.Proper(value) =>
          Schema.Labeled.Unlabeled:
            value.wipeTranslate[Schema.Labeled[String, _]]([X] => pso => Exists(pso.resolve(alreadyResolved)))
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
        case Oriented.Unsupported(details) =>
          Schema.Labeled.Unsupported(SingletonType(details))
  }

  private trait Resolver[A, E, F[_]] {
    def resolve(name: String): F[Either[E, A]]
  }

  private type TopoSortF[I, O, A] = ReaderState[List[String], (List[(String, O)], List[(String, I)]), A]

  private case class Cycle(schemas: List[String])

  private object TopoSortF {
    def resolver[I, O](
      elemResolver: I => TopoSortF[I, O, O],
      reportCycle: (String, List[String]) => O,
    ): Resolver[O, Option[Cycle], TopoSortF[I, O, _]] =
      new Resolver[O, Option[Cycle], TopoSortF[I, O, _]] {
        override def resolve(name: String): TopoSortF[I, O, Either[Option[Cycle], O]] =
          ReaderState:
            case (dependents, (acc, remaining)) =>
              val i = dependents.indexOf(name)
              if (i >= 0) {
                val cycle = dependents.head :: dependents.take(i+1).reverse
                ((acc, remaining), Left(Some(Cycle(cycle))))
              } else {
                acc.collectFirst { case (`name`, o) => o } match
                  case Some(o) =>
                    ((acc, remaining), Right(o))
                  case None =>
                    remaining.collectFirst { case (`name`, i) => i } match
                      case None =>
                        ((acc, remaining), Left(None))
                      case Some(i) =>
                        val ((acc1, remaining1), o) =
                          elemResolver(i).run(name :: dependents, (acc, remaining.filter { case (n, _) => n != name }))
                        ((acc1 :+ (name, o), remaining1), Right(o))
              }
      }

    val protoResolver: Resolver[ProtoSchema.Oriented, Option[Cycle], TopoSortF[ProtoSchema, ProtoSchema.Oriented, _]] =
      new Resolver[ProtoSchema.Oriented, Option[Cycle], TopoSortF[ProtoSchema, ProtoSchema.Oriented, _]] { self =>
        val elemResolver: ProtoSchema => TopoSortF[ProtoSchema, ProtoSchema.Oriented, ProtoSchema.Oriented] =
          _.resolveA(using self)

        val delegate = resolver(elemResolver, reportCycle = Oriented.ForwardRef(_, _))

        override def resolve(name: String): TopoSortF[ProtoSchema, Oriented, Either[Option[Cycle], Oriented]] =
          delegate.resolve(name)
      }
  }

  private def sortTopologically(schemas: List[(String, ProtoSchema)]): List[(String, ProtoSchema.Oriented)] = {
    @tailrec
    def go(
      acc: List[(String, Oriented)],
      remaining: List[(String, ProtoSchema)]
    ): List[(String, Oriented)] =
      remaining match {
        case Nil =>
          acc
        case (name, schema) :: tail =>
          val ((acc1, remaining1), oriented) =
            schema
              .resolveA(using TopoSortF.protoResolver)
              .run(List(name), (acc, tail))
          go(acc1 :+ (name, oriented), remaining1)
      }

    go(acc = Nil, remaining = schemas)
  }

  def resolveAcyclic(schemas: List[(String, ProtoSchema)]): List[(String, Schema.Labeled[String, ?])] = {
    @tailrec
    def go(
      revAcc: List[(String, Schema.Labeled[String, ?])],
      accMap: Map[String, Schema.Labeled[String, ?]], // contains the same elements as revAcc
      remaining: List[(String, ProtoSchema.Oriented)],
    ): List[(String, Schema.Labeled[String, ?])] =
      remaining match
        case (name, schema) :: tail =>
          val resolved = schema.resolve(accMap)
          go((name, resolved) :: revAcc, accMap.updated(name, resolved), tail)
        case Nil =>
          revAcc.reverse

    go(Nil, Map.empty, sortTopologically(schemas))
  }

}
