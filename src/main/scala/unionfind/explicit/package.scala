package unionfind

package object explicit extends HelperSpecific {
  def typeCheck(tuple: (Expr, TypeUnionFind), tyEnv: TypeEnv): Type = {
    val (expr, tuf) = tuple
    expr match {
      case Num(_) => NumT
      case Add(l, r) =>
        tuf.union(typeCheck((l, tuf), tyEnv), NumT)
        tuf.union(typeCheck((r, tuf), tyEnv), NumT)
        NumT
      case Sub(l, r) =>
        tuf.union(typeCheck((l, tuf), tyEnv), NumT)
        tuf.union(typeCheck((r, tuf), tyEnv), NumT)
        NumT
      case Id(x) =>
        tyEnv.getOrElse(x, notype(s"$x is a free identifier"))
      case Fun(p, t, b) =>
        //validType(t, tyEnv)
        ArrowT(t, typeCheck((b, tuf), tyEnv + (p -> t)))
      case App(f, a) =>
        val resT = VarT(tuf.newVarTNum())
        tuf.makeSet(resT)
        tuf.union(ArrowT(typeCheck((a, tuf), tyEnv), resT), typeCheck((f, tuf), tyEnv))
        resT
      case If0(c, t, f) =>
        tuf.union(typeCheck((c, tuf), tyEnv), NumT)
        val ty = typeCheck((t, tuf), tyEnv)
        tuf.union(ty, typeCheck((f, tuf), tyEnv))
        ty
      case Rec(f, ft, x, xt, b) =>
        //validType(ft, tyEnv)
        //validType(xt, tyEnv)
        val newTyEnv = tyEnv + (f -> ft) + (x -> xt)
        tuf.union(ft, ArrowT(xt, typeCheck((b, tuf), newTyEnv)))
        ft
    }
  }

  def interpret(expr: Expr, env: Env): Value = ???

  def run(str: String): String = {
    val (expr, tuf) = TIRCFAE(str)
    typeCheck((expr, tuf), Map())
    interpret(expr, Map()).toString
  }

  def tests: Unit = {
    def testTypeCheck(str: String, output: String): Unit = {
      val (expr, tuf) = TIRCFAE(str)
      test(typeCheck((expr, tuf), Map()).toStringWith(tuf), output)
    }

    def testExcTypeCheck(str: String, msg: String): Unit = {
      val (expr, tuf) = TIRCFAE(str)
      testExc(typeCheck((expr, tuf), Map()).toStringWith(tuf), msg)
    }
    
    testTypeCheck("{fun {x : ?} {+ x 1}}", "(num -> num)")
    testTypeCheck("{fun {y : ?} y}", "(? -> ?)")
    testTypeCheck("{{fun {y : ?} y} {fun {x : ?} {+ x 1}}}", "(num -> num)")
    testTypeCheck("{fun {y : ?} {y 7}}", "((num -> ?) -> ?)")
    testExcTypeCheck("{fun {x : ?} {x x}}", "cyclic type")
  }
}
