package jing.macroUtil

import scala.NamedTuple.NamedTuple
import scala.annotation.experimental
import scala.quoted.*

object TupledFunctions {
  /** Returns a function that takes each element of a named tuple as a separate, correspondingly named parameter.
   *
   * **Caveats:**
   *
   * - Not yet working for more than 22 endpoints, due to https://github.com/scala/scala3/issues/23313.
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
   * - Not yet working for more than 22 endpoints, due to https://github.com/scala/scala3/issues/23313.
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
    ).asExpr
  }

  // Could be shortened using quoted type patterns ('[t]) if https://github.com/scala/scala3/issues/23317 was resolved
  // and even more significantly simplified if https://github.com/scala/scala3/issues/23313 was resolved.
  private def formTuple(
    args: List[Expr[Any]],
    types: List[Type[? <: Any]],
  )(using Quotes): Expr[Any] = {
    require(args.size == types.size)

    (args zip types) match
      case Nil =>
        '{ EmptyTuple }
      case (a1, t1) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        '{ Tuple1(${a1.asExprOf(using t1)}) }
      case (a1, t1) :: (a2, t2) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: (a14, t14) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        given tp14: Type[t14.Underlying] = t14
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
          ${a14.asExprOf(using t14)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: (a14, t14) :: (a15, t15) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        given tp14: Type[t14.Underlying] = t14
        given tp15: Type[t15.Underlying] = t15
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
          ${a14.asExprOf(using t14)},
          ${a15.asExprOf(using t15)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: (a14, t14) :: (a15, t15) :: (a16, t16) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        given tp14: Type[t14.Underlying] = t14
        given tp15: Type[t15.Underlying] = t15
        given tp16: Type[t16.Underlying] = t16
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
          ${a14.asExprOf(using t14)},
          ${a15.asExprOf(using t15)},
          ${a16.asExprOf(using t16)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: (a14, t14) :: (a15, t15) :: (a16, t16) :: (a17, t17) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        given tp14: Type[t14.Underlying] = t14
        given tp15: Type[t15.Underlying] = t15
        given tp16: Type[t16.Underlying] = t16
        given tp17: Type[t17.Underlying] = t17
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
          ${a14.asExprOf(using t14)},
          ${a15.asExprOf(using t15)},
          ${a16.asExprOf(using t16)},
          ${a17.asExprOf(using t17)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: (a14, t14) :: (a15, t15) :: (a16, t16) :: (a17, t17) :: (a18, t18) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        given tp14: Type[t14.Underlying] = t14
        given tp15: Type[t15.Underlying] = t15
        given tp16: Type[t16.Underlying] = t16
        given tp17: Type[t17.Underlying] = t17
        given tp18: Type[t18.Underlying] = t18
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
          ${a14.asExprOf(using t14)},
          ${a15.asExprOf(using t15)},
          ${a16.asExprOf(using t16)},
          ${a17.asExprOf(using t17)},
          ${a18.asExprOf(using t18)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: (a14, t14) :: (a15, t15) :: (a16, t16) :: (a17, t17) :: (a18, t18) :: (a19, t19) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        given tp14: Type[t14.Underlying] = t14
        given tp15: Type[t15.Underlying] = t15
        given tp16: Type[t16.Underlying] = t16
        given tp17: Type[t17.Underlying] = t17
        given tp18: Type[t18.Underlying] = t18
        given tp19: Type[t19.Underlying] = t19
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
          ${a14.asExprOf(using t14)},
          ${a15.asExprOf(using t15)},
          ${a16.asExprOf(using t16)},
          ${a17.asExprOf(using t17)},
          ${a18.asExprOf(using t18)},
          ${a19.asExprOf(using t19)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: (a14, t14) :: (a15, t15) :: (a16, t16) :: (a17, t17) :: (a18, t18) :: (a19, t19) :: (a20, t20) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        given tp14: Type[t14.Underlying] = t14
        given tp15: Type[t15.Underlying] = t15
        given tp16: Type[t16.Underlying] = t16
        given tp17: Type[t17.Underlying] = t17
        given tp18: Type[t18.Underlying] = t18
        given tp19: Type[t19.Underlying] = t19
        given tp20: Type[t20.Underlying] = t20
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
          ${a14.asExprOf(using t14)},
          ${a15.asExprOf(using t15)},
          ${a16.asExprOf(using t16)},
          ${a17.asExprOf(using t17)},
          ${a18.asExprOf(using t18)},
          ${a19.asExprOf(using t19)},
          ${a20.asExprOf(using t20)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: (a14, t14) :: (a15, t15) :: (a16, t16) :: (a17, t17) :: (a18, t18) :: (a19, t19) :: (a20, t20) :: (a21, t21) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        given tp14: Type[t14.Underlying] = t14
        given tp15: Type[t15.Underlying] = t15
        given tp16: Type[t16.Underlying] = t16
        given tp17: Type[t17.Underlying] = t17
        given tp18: Type[t18.Underlying] = t18
        given tp19: Type[t19.Underlying] = t19
        given tp20: Type[t20.Underlying] = t20
        given tp21: Type[t21.Underlying] = t21
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
          ${a14.asExprOf(using t14)},
          ${a15.asExprOf(using t15)},
          ${a16.asExprOf(using t16)},
          ${a17.asExprOf(using t17)},
          ${a18.asExprOf(using t18)},
          ${a19.asExprOf(using t19)},
          ${a20.asExprOf(using t20)},
          ${a21.asExprOf(using t21)},
        ) }
      case (a1, t1) :: (a2, t2) :: (a3, t3) :: (a4, t4) :: (a5, t5) :: (a6, t6) :: (a7, t7) :: (a8, t8) :: (a9, t9) :: (a10, t10) :: (a11, t11) :: (a12, t12) :: (a13, t13) :: (a14, t14) :: (a15, t15) :: (a16, t16) :: (a17, t17) :: (a18, t18) :: (a19, t19) :: (a20, t20) :: (a21, t21) :: (a22, t22) :: Nil =>
        given tp1: Type[t1.Underlying] = t1
        given tp2: Type[t2.Underlying] = t2
        given tp3: Type[t3.Underlying] = t3
        given tp4: Type[t4.Underlying] = t4
        given tp5: Type[t5.Underlying] = t5
        given tp6: Type[t6.Underlying] = t6
        given tp7: Type[t7.Underlying] = t7
        given tp8: Type[t8.Underlying] = t8
        given tp9: Type[t9.Underlying] = t9
        given tp10: Type[t10.Underlying] = t10
        given tp11: Type[t11.Underlying] = t11
        given tp12: Type[t12.Underlying] = t12
        given tp13: Type[t13.Underlying] = t13
        given tp14: Type[t14.Underlying] = t14
        given tp15: Type[t15.Underlying] = t15
        given tp16: Type[t16.Underlying] = t16
        given tp17: Type[t17.Underlying] = t17
        given tp18: Type[t18.Underlying] = t18
        given tp19: Type[t19.Underlying] = t19
        given tp20: Type[t20.Underlying] = t20
        given tp21: Type[t21.Underlying] = t21
        given tp22: Type[t22.Underlying] = t22
        '{ (
          ${a1.asExprOf(using t1)},
          ${a2.asExprOf(using t2)},
          ${a3.asExprOf(using t3)},
          ${a4.asExprOf(using t4)},
          ${a5.asExprOf(using t5)},
          ${a6.asExprOf(using t6)},
          ${a7.asExprOf(using t7)},
          ${a8.asExprOf(using t8)},
          ${a9.asExprOf(using t9)},
          ${a10.asExprOf(using t10)},
          ${a11.asExprOf(using t11)},
          ${a12.asExprOf(using t12)},
          ${a13.asExprOf(using t13)},
          ${a14.asExprOf(using t14)},
          ${a15.asExprOf(using t15)},
          ${a16.asExprOf(using t16)},
          ${a17.asExprOf(using t17)},
          ${a18.asExprOf(using t18)},
          ${a19.asExprOf(using t19)},
          ${a20.asExprOf(using t20)},
          ${a21.asExprOf(using t21)},
          ${a22.asExprOf(using t22)},
        ) }
      case argsAndTypes =>
        formTupleNAry(argsAndTypes) // this does not work yet due to the following bug:
        Expr(s"More than 22 endpoints (was ${args.size}) not supported due to https://github.com/scala/scala3/issues/23313.")
  }

  private def formTupleNAry(
    argsAndTypes: List[(Expr[Any], Type[?])],
  )(using Quotes): Expr[Any] = {
    sealed trait TupleAcc {
      type T <: Tuple
      val expr: Expr[T]
      val tpe: Type[T]
    }
    case class TupleAccImpl[U <: Tuple](override val expr: Expr[U], override val tpe: Type[U]) extends TupleAcc {
      override type T = U
    }

    argsAndTypes
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
