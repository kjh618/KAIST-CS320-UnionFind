package unionfind

import `implicit`.TypeUnification._

package object `implicit` extends HelperSpecific {
  def typeCheck(expr: Expr, tyEnv: TypeEnv): Type = expr match {
    case Num(_) => NumT
    case Add(l, r) =>
      unify(typeCheck(l, tyEnv), NumT)
      unify(typeCheck(r, tyEnv), NumT)
      NumT
    case Sub(l, r) =>
      unify(typeCheck(l, tyEnv), NumT)
      unify(typeCheck(r, tyEnv), NumT)
      NumT
    case Id(x) =>
      tyEnv.getOrElse(x, notype(s"$x is a free identifier"))
    case Fun(p, t, b) =>
      //validType(t, tyEnv)
      ArrowT(t, typeCheck(b, tyEnv + (p -> t)))
    case App(f, a) =>
      val resT = VarT(None)
      unify(ArrowT(typeCheck(a, tyEnv), resT), typeCheck(f, tyEnv))
      resT
    case If0(c, t, f) =>
      unify(typeCheck(c, tyEnv), NumT)
      val ty = typeCheck(t, tyEnv)
      unify(ty, typeCheck(f, tyEnv))
      ty
    case Rec(f, ft, x, xt, b) =>
      //validType(ft, tyEnv)
      //validType(xt, tyEnv)
      val newTyEnv = tyEnv + (f -> ft) + (x -> xt)
      unify(ft, ArrowT(xt, typeCheck(b, newTyEnv)))
      ft
  }

  def interpret(expr: Expr, env: Env): Value = ???

  def run(str: String): String = {
    val expr = TIRCFAE(str)
    typeCheck(expr, Map())
    interpret(expr, Map()).toString
  }

  def tests: Unit = {
    test(typeCheck(TIRCFAE("{fun {x : ?} {+ x 1}}"), Map()).toString, "(num -> num)")
    test(typeCheck(TIRCFAE("{fun {y : ?} y}"), Map()).toString, "(? -> ?)")
    test(typeCheck(TIRCFAE("{{fun {y : ?} y} {fun {x : ?} {+ x 1}}}"), Map()).toString, "(num -> num)")
    test(typeCheck(TIRCFAE("{fun {y : ?} {y 7}}"), Map()).toString, "((num -> ?) -> ?)")
    testExc(typeCheck(TIRCFAE("{fun {x : ?} {x x}}"), Map()).toString, "cyclic type")
  }
}
