package jing.macroUtil

import scala.NamedTuple.NamedTuple
import scala.annotation.experimental
import scala.quoted.*

object TupledFunctions {
  /** Returns a function that takes each element of a named tuple as a separate, correspondingly named parameter.
   *
   * **Caveats:**
   *
   * - Not getting IDE hints for individual parameters, due to https://github.com/scalameta/metals/issues/7532.
   *
   * - Relies on internal compiler APIs to synthesize the function type of arbitrary arity.
   *   (https://github.com/scala/scala3/discussions/23326)
   *
   */
  transparent inline def untupled[Ns <: Tuple, Ts <: Tuple, R](f: NamedTuple[Ns, Ts] => R) =
    ${ untupledImpl[Ns, Ts, R]('f) }

  /** Returns a structurally typed object with an `apply` method that takes each element of a named tuple as a separate,
   *  correspondingly named parameter.
   *
   * Functionally equivalent to [[namedUntupled]], with the upside of not relying on compiler internals, at the cost of structural typing.
   *
   * **Caveats:**
   *
   * - Not getting IDE hints for individual parameters, due to https://github.com/scalameta/metals/issues/7537.
   *
   */
  @experimental("Has experimental dependencies")
  transparent inline def untupledMethod[Ns <: Tuple, Ts <: Tuple, R](f: NamedTuple[Ns, Ts] => R) =
    ${ untupledMethodImpl[Ns, Ts, R]('f) }

  private def untupledImpl[Ns <: Tuple, Ts <: Tuple, R](
    f: Expr[NamedTuple[Ns, Ts] => R],
  )(using Quotes, Type[Ns], Type[Ts], Type[R]): Expr[Any] = {
    import quotes.reflect.*

    val paramNames = constantStringTypes[Ns]
    val paramTypes = tupleElems[Ts]
    assert(paramNames.size == paramTypes.size)

    val methodType =
      MethodType(paramNames = paramNames)(
        mt => paramTypes.map(TypeRepr.of(using _)),
        mt => TypeRepr.of[R],
      )
    val methodSym =
      Symbol.newMethod(
        Symbol.spliceOwner,
        name = "untupled",
        tpe = methodType,
      )
    val methodDef =
      DefDef(
        methodSym,
        rhsFn = { case List(argTrees) =>
          given Quotes = methodSym.asQuotes
          val args: List[Expr[Any]] = argTrees.map(_.asInstanceOf[Term].asExpr)
          Some(
            Select.unique(f.asTerm, "apply")
              .appliedTo(
                formTuple(args, paramTypes)
                  .asExprOf[Ts]
                  .asTerm
              )
          )
        },
      )

    // XXX: relying on compiler internals to obtain the corresponding FunctionN type.
    val functionType =
      methodType
        .asInstanceOf[dotty.tools.dotc.core.Types.Type]
        .toFunctionType()(using quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx)
        .asInstanceOf[TypeRepr]

    // refine the `apply` method in order to preserve parameter names
    val refinedFunctionType =
      Refinement(
        functionType,
        "apply",
        methodType,
      )

    Block(
      List(methodDef),
      Closure(Ident(methodSym.termRef), tpe = Some(refinedFunctionType))
    ).asExpr
  }

  @experimental("Has experimental dependencies")
  def untupledMethodImpl[Ns <: Tuple, Ts <: Tuple, R](
    f: Expr[NamedTuple[Ns, Ts] => R],
  )(using Quotes, Type[Ns], Type[Ts], Type[R]): Expr[Any] = {
    import quotes.reflect.*
    import StructuralRefinement.MemberDef

    val paramNames = constantStringTypes[Ns]
    val paramTypes = tupleElems[Ts]
    assert(paramNames.size == paramTypes.size)

    StructuralRefinement.typedTerm[AnyRef][quotes.type](
      owner = Symbol.spliceOwner,
      members = List(
        "apply" -> MemberDef.poly[quotes.type, "term-synth"] { [M] => (m, _) ?=> _ =>
          MemberDef.Method(
            MethodType(paramNames = paramNames)(
              mt => paramTypes.map(TypeRepr.of(using _)),
              mt => TypeRepr.of[R],
            ),
            body = m.isTermSynth.map { _ => (self, argss) =>
              given Quotes = self.asQuotes
              val List(argTrees) = argss
              val args: List[Expr[Any]] = argTrees.map(_.asInstanceOf[Term].asExpr)
              Select.unique(f.asTerm, "apply")
                .appliedTo(
                  formTuple(args, paramTypes)
                    .asTerm
                )
            }
          )
        }
      ),
      "UntupledFunction",
      path = "",
      debugPrint = _ => (),
    ).asExpr
  }

  private def formTuple(
    args: List[Expr[Any]],
    types: List[Type[? <: Any]],
  )(using Quotes): Expr[Any] = {
    require(args.size == types.size)

    sealed trait TupleAcc {
      type T <: Tuple
      val expr: Expr[T]
      val tpe: Type[T]
    }
    case class TupleAccImpl[U <: Tuple](override val expr: Expr[U], override val tpe: Type[U]) extends TupleAcc {
      override type T = U
    }

    (args zip types)
      .foldRight[TupleAcc](TupleAccImpl[EmptyTuple]('{ EmptyTuple }, Type.of[EmptyTuple])) {
        case ((arg, '[t]), acc) =>
          val accExp: Expr[acc.T] = acc.expr
          given Type[acc.T] = acc.tpe
          TupleAccImpl[t *: acc.T](
            '{ ${arg.asExprOf[t]} *: ${accExp} },
            Type.of[t *: acc.T]
          )

      }
      .expr
  }

  private def constantStringTypes[Ns <: Tuple](using Quotes, Type[Ns]): List[String] =
    import quotes.reflect.*

    Type.of[Ns] match
      case '[EmptyTuple] => Nil
      case '[n *: ns] =>
        TypeRepr.of[n] match
          case ConstantType(StringConstant(n)) => n :: constantStringTypes[ns]

  private def tupleElems[Ts <: Tuple](using Quotes, Type[Ts]): List[Type[? <: Any]] =
    Type.of[Ts] match
      case '[EmptyTuple] => Nil
      case '[t *: ts] => Type.of[t] :: tupleElems[ts]
}
