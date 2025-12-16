package jing.openapi

import jing.openapi.model.{||, Arr, Bool, Const, DiscriminatedUnion, Enum, Int32, Int64, Obj, ScalaValueOf, Schema, SchemaMotif, Str}
import jing.openapi.model.IsPropertyOf.IsRequiredPropertyOf
import libretto.lambda.Items1Named
import libretto.lambda.util.{Applicative, Exists, SingletonType, Validated}
import libretto.lambda.util.Applicative.traverseList
import libretto.lambda.util.Exists.Indeed
import libretto.lambda.util.NonEmptyList
import libretto.lambda.util.Validated.{Invalid, Valid, invalid, valid}
import scala.annotation.tailrec

/** Schema with unresolved references to other schemas. */
private[openapi] enum ProtoSchema[A] {
  case Proper(value: SchemaMotif[ProtoSchema, A])
  case OneOf(
    discriminatorProperty: String,
    schemas: List[ProtoSchema[?]],
    declaredMapping: Option[Map[String, String]],
  ) extends ProtoSchema[DiscriminatedUnion[Unknown]]
  case Ref(schemaName: String) extends ProtoSchema[Unknown]
  case Unsupported(details: String) extends ProtoSchema[Unknown]

  import ProtoSchema.*

  def resolve(
    alreadyResolved: Map[String, Schema.Labeled[String, ?]],
  ): Schema.Labeled[String, ?] =
    resolveA[[x] =>> x](using
      Resolver.fromMap(alreadyResolved, notFound = Option.empty[Cycle])
    )

  private def resolveA[F[_]](using
    r: Resolver[Schema.Labeled[String, ?], Option[Cycle], F],
    F: Applicative[F],
  ): F[Schema.Labeled[String, ?]] =
    this match
      case Proper(value) =>
        value
          .wipeTranslateA[F, Schema.Labeled[String, _]]([X] => ps => ps.resolveA[F].map(Exists(_)))
          .map(Schema.Labeled.Unlabeled(_))
      case OneOf(discriminatorProperty, protoSchemas, declaredMapping) =>
        Applicative
          .traverseList(protoSchemas)(_.resolveA[F])
          .map(resolveOneOf(discriminatorProperty, _, declaredMapping))
      case Ref(schemaName) =>
        r.resolve(schemaName) map:
          case Right(schema) =>
            Schema.Labeled.WithLabel(schemaName, schema)
          case Left(None) =>
            val msg = s"Unresolved schema $schemaName"
            Schema.Labeled.Unsupported(SingletonType(msg))
          case Left(Some(Cycle(cycle))) =>
            val msg = s"Unsupported recursive schema: ${cycle.mkString(" -> ")}"
            Schema.Labeled.Unsupported(SingletonType(msg))
      case Unsupported(details) =>
        F.pure(Schema.Labeled.Unsupported(SingletonType(details)))
}

private[openapi] object ProtoSchema {
  import jing.openapi.model.{SchemaMotif as motif}

  def i32: ProtoSchema[Int32] = Proper(motif.I32())
  def i64: ProtoSchema[Int64] = Proper(motif.I64())
  def str: ProtoSchema[Str] = Proper(motif.S())
  def bool: ProtoSchema[Bool] = Proper(motif.B())

  def strEnum(value: String, values: String*): Exists[[cases] =>> ProtoSchema[Enum[Str, cases]]] =
    Indeed(Proper(SchemaMotif.Enumeration.str[ProtoSchema](value, values*)))

  def int32Enum(value: Int, values: Int*): Exists[[cases] =>> ProtoSchema[Enum[Int32, cases]]] =
    Indeed(Proper(SchemaMotif.Enumeration.int32(value, values*)))

  def int64Enum(value: Long, values: Long*): Exists[[cases] =>> ProtoSchema[Enum[Int64, cases]]] =
    Indeed(Proper(SchemaMotif.Enumeration.int64(value, values*)))

  def boolEnum(value: Boolean, values: Boolean*): Exists[[cases] =>> ProtoSchema[Enum[Bool, cases]]] =
    Indeed(Proper(SchemaMotif.Enumeration.bool(value, values*)))

  def strConst(value: String): ProtoSchema[Const[value.type]] =
    Proper(SchemaMotif.Constant.Primitive(ScalaValueOf.str(value)))

  def int32Const(value: Int): ProtoSchema[Const[value.type]] =
    Proper(SchemaMotif.Constant.Primitive(ScalaValueOf.i32(value)))

  def int64Const(value: Long): ProtoSchema[Const[value.type]] =
    Proper(SchemaMotif.Constant.Primitive(ScalaValueOf.i64(value)))

  def boolConst(value: Boolean): ProtoSchema[Const[value.type]] =
    Proper(SchemaMotif.Constant.Primitive(ScalaValueOf.bool(value)))

  def arr[A](elemSchema: ProtoSchema[A]): ProtoSchema[Arr[A]] =
    Proper(motif.Array(elemSchema))

  def obj(props: List[(String, Boolean, ProtoSchema[?])]): Exists[[props] =>> ProtoSchema[Obj[props]]] = {
    @tailrec
    def go(
      acc: motif.Object[ProtoSchema, ?],
      remaining: List[(String, Boolean, ProtoSchema[?])],
    ): motif.Object[ProtoSchema, ?] =
      remaining match
        case Nil =>
          acc
        case (n, isRequired, s) :: ps =>
          if isRequired
          then go(motif.Object.snoc(acc, n, s), ps)
          else go(motif.Object.snocOpt(acc, n, s), ps)

    Indeed(Proper(go(motif.Object.empty, props)))
  }

  private case class Cycle(schemas: List[String])

  private trait Resolver[A, E, F[_]] {
    def resolve(name: String): F[Either[E, A]]
  }

  private object Resolver {
    def fromMap[A, NotFound](
      m: Map[String, A],
      notFound: NotFound,
    ): Resolver[A, NotFound, [x] =>> x] =
      new Resolver[A, NotFound, [x] =>> x]:
        override def resolve(name: String): Either[NotFound, A] =
          m.get(name).toRight(left = notFound)
  }

  private type TopoSortF[I, O, A] = ReaderState[List[String], (List[(String, O)], List[(String, I)]), A]

  private object TopoSortF {
    def resolver[I, O](
      elemResolver: I => TopoSortF[I, O, O],
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

    val protoResolver: Resolver[Schema.Labeled[String, ?], Option[Cycle], TopoSortF[ProtoSchema[?], Schema.Labeled[String, ?], _]] =
      new Resolver[Schema.Labeled[String, ?], Option[Cycle], TopoSortF[ProtoSchema[?], Schema.Labeled[String, ?], _]] { self =>
        val elemResolver: ProtoSchema[?] => TopoSortF[ProtoSchema[?], Schema.Labeled[String, ?], Schema.Labeled[String, ?]] =
          _.resolveA(using self)

        val delegate = resolver(elemResolver)

        override def resolve(name: String): TopoSortF[ProtoSchema[?], Schema.Labeled[String, ?], Either[Option[Cycle], Schema.Labeled[String, ?]]] =
          delegate.resolve(name)
      }
  }

  def resolveAcyclic(schemas: List[(String, ProtoSchema[?])]): List[(String, Schema.Labeled[String, ?])] = {
    @tailrec
    def go(
      acc: List[(String, Schema.Labeled[String, ?])],
      remaining: List[(String, ProtoSchema[?])]
    ): List[(String, Schema.Labeled[String, ?])] =
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

  private def resolveOneOf(
    discriminatorProperty: String,
    cases: List[Schema.Labeled[String, ?]],
    declaredMapping: Option[Map[String, String]], // TODO: check against reality
  ): Schema.Labeled[String, ?] = {
    val schemasByDiscriminatorValues: Validated[String, List[(String, Schema.Labeled[String, ?], Int)]] =
      traverseList(cases.zipWithIndex):
        case (s, i) =>
          Validated.fromEither:
            extractDiscriminatorValue(discriminatorProperty, s)
              .map(r => (r.value._3, s, i))
              .left.map(err => s"Case ${i+1}: $err")
    schemasByDiscriminatorValues match
      case Invalid(errors) =>
        val msg = errors.toList.map(e => if (e.last == '.') e else s"$e.").mkString(" ")
        Schema.Labeled.unsupported(msg)
      case Valid(schemasByDiscriminatorValues) =>
        val discriminatorOccurrences: Map[String, List[Int]] =
          schemasByDiscriminatorValues.groupMap(_._1)(_._3)
        discriminatorOccurrences.collectFirst { case x @ (_, _ :: _ :: _) => x } match
          case Some((value, occurrences)) =>
            Schema.Labeled.unsupported(s"Ambiguous oneOf schema: \"$discriminatorProperty\" = \"$value\" occurs in cases ${occurrences.map(_ + 1).mkString(", ")}.")
          case None =>
            assert(discriminatorOccurrences.size == schemasByDiscriminatorValues.size)
            NonEmptyList.fromList(schemasByDiscriminatorValues) match
              case None =>
                Schema.Labeled.unsupported("oneOf with 0 cases is not supported")
              case Some(nel) =>
                validateDiscriminatorMapping(discriminatorProperty, nel, declaredMapping) match
                  case Left(err) => Schema.Labeled.unsupported(err)
                  case Right(()) =>
                    Schema.Labeled.Unlabeled(
                      SchemaMotif.OneOf(
                        discriminatorProperty,
                        Items1Named.Product.fromList(nel.map { case (k, v, _) => (k, v) }.toList)[Schema.Labeled[String, _]]([R] => (s, f) => f(s))[||, model.::],
                      )
                    )
  }

  private def validateDiscriminatorMapping(
    discriminatorProperty: String,
    schemasByDiscriminatorValues: NonEmptyList[(String, Schema.Labeled[String, ?], Int)],
    mapping: Option[Map[String, String]],
  ): Either[String, Unit] =
    mapping match {
      case None =>
        Right(())

      case Some(mapping) =>
        val mappingValid: Validated[String, ?] =
          traverseList(mapping.toList)[Validated[String, _], Unit]:
            case (discriminatorValue, schemaName) =>
              schemasByDiscriminatorValues.toList.find(_._2.labelOpt.contains(schemaName)) match
                case None =>
                  invalid(s"Mapping of \"$discriminatorProperty\": \"$discriminatorValue\" refers to schema $schemaName, which is not among the oneOf cases.")
                case Some((targetDiscriminatorValue, _, _)) =>
                  if (targetDiscriminatorValue != discriminatorValue)
                    invalid(s"Mapping of \"$discriminatorProperty\": \"$discriminatorValue\" refers to schema $schemaName, which, however, defines \"$discriminatorProperty\": \"$targetDiscriminatorValue\".")
                  else
                    valid(())
        val mappingComplete: Validated[String, ?] =
          schemasByDiscriminatorValues.traverse:
            case (discriminatorValue, schema, idx) =>
              if (mapping.contains(discriminatorValue))
                valid(())
              else
                val schemaNameStr = schema.labelOpt.map(schemaName => s" ($schemaName)").getOrElse("")
                invalid(s"Mapping is missing entry for \"$discriminatorProperty\": \"$discriminatorValue\", defined by oneOf case ${idx+1}$schemaNameStr.")
        (mappingValid zip mappingComplete) match
          case Invalid(errors) => Left(errors.toList.mkString(" "))
          case Valid(_) => Right(())
    }

  private def extractDiscriminatorValue[A](
    discriminatorProperty: String,
    schema: Schema.Labeled[String, A],
  ): Either[String, Exists[[Ps] =>> (
    A =:= Obj[Ps],
    discriminatorProperty.type IsRequiredPropertyOf Ps,
    String, // the unique value of discriminatorProperty
  )]] =
    schema match
      case Schema.Labeled.Unlabeled(schema) =>
        schema match
          case obj: motif.Object[sl, ps] =>
            extractDiscriminatorValue(discriminatorProperty, obj)
              .map { case (i, propConstantValue) => Exists((summon[A =:= Obj[ps]], i, propConstantValue)) }

          case other =>
            Left(s"Expected object with property $discriminatorProperty, found ${shortTypeName(other)}")

      case Schema.Labeled.WithLabel(label, schema) =>
        extractDiscriminatorValue(discriminatorProperty, schema)
          .left
          .map(err => s"$label: $err")

      case Schema.Labeled.Unsupported(message) =>
        Left(message.value)

  private def extractDiscriminatorValue[Ps](
    discriminatorProperty: String,
    objSchema: SchemaMotif.Object[Schema.Labeled[String, _], Ps],
  ): Either[String, (
    discriminatorProperty.type IsRequiredPropertyOf Ps,
    String, // the unique value of discriminatorProperty
  )] =
    objSchema.value.getOptFull(discriminatorProperty) match
      case Some(Indeed(Left((i, propSchema)))) =>
        asConstantString(propSchema) match
          case Right(propConstantValue) =>
            Right((i, propConstantValue))
          case Left(actual) =>
            Left(s"discriminator property \"$discriminatorProperty\" must have a constant string type (const or enum with a single case), but was $actual")
      case Some(Indeed(Right(_))) =>
        Left(s"discriminator property \"$discriminatorProperty\" must be a required property, but is not")
      case None =>
        Left(s"object is missing the discriminator property \"$discriminatorProperty\"")

  private def asConstantString[A](
    schema: Schema.Labeled[String, A],
  ): Either[String, String] =
    schema match
      case Schema.Labeled.WithLabel(label, schema) =>
        asConstantString(schema)
      case Schema.Labeled.Unlabeled(schema) =>
        schema match
          case enm @ motif.Enumeration(_, _) =>
            asConstantString(enm)
          case motif.Constant.Primitive(ScalaValueOf.S(s)) =>
            Right(s.value)
          case motif.S() =>
            Left(s"unconstrained string")
          case other =>
            Left(shortTypeName(other))
      case Schema.Labeled.Unsupported(message) =>
        Left(s"Unsupported: ${message.value}")

  private def asConstantString[F[_], Base, Cases](
    enm: motif.Enumeration[F, Base, Cases]
  ): Either[String, String] =
    val motif.Enumeration(base, cases) = enm
    base match
      case motif.S() =>
        summon[Base =:= Str]
        cases.toList([s] => v => v.get) match
          case s :: Nil => Right(s)
          case ss @ (_ :: _ :: _) => Left(s"enum with ${ss.size} cases (${ss.mkString("\"", ", ", "\"")})")
      case other =>
        Left(shortTypeName(other))

  private def shortTypeName[F[_], A](
    schema: SchemaMotif[F, A],
  ): String =
    schema match
      case motif.I32() => "integer"
      case motif.I64() => "integer"
      case motif.S() => "string"
      case motif.B() => "boolean"
      case motif.Enumeration(_, _) => "enum"
      case motif.Array(_) => "array"
      case motif.OneOf(_, _) => "oneOf"
      case motif.Object(_) => "object"
      case motif.Constant.Primitive(value) => s"const ${value.show}"
}
