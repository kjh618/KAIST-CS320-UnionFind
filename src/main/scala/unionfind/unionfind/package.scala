package unionfind

package object unionfind extends HelperUnionFind {
  def typeCheck(tuple: (Expr, UnionFind), tyEnv: TypeEnv): Type = {
    val (expr, uf) = tuple
    expr match {
      case Num(_) => NumT
      case Add(l, r) =>
        uf.union(typeCheck((l, uf), tyEnv), NumT)
        uf.union(typeCheck((r, uf), tyEnv), NumT)
        NumT
      case Sub(l, r) =>
        uf.union(typeCheck((l, uf), tyEnv), NumT)
        uf.union(typeCheck((r, uf), tyEnv), NumT)
        NumT
      case Id(x) =>
        tyEnv.getOrElse(x, notype(s"$x is a free identifier"))
      case Fun(p, t, b) =>
        //validType(t, tyEnv)
        ArrowT(t, typeCheck((b, uf), tyEnv + (p -> t)))
      case App(f, a) =>
        val resT = VarT(uf.newVarTNum())
        uf.makeSet(resT)
        uf.union(ArrowT(typeCheck((a, uf), tyEnv), resT), typeCheck((f, uf), tyEnv))
        resT
      case If0(c, t, f) =>
        uf.union(typeCheck((c, uf), tyEnv), NumT)
        val ty = typeCheck((t, uf), tyEnv)
        uf.union(ty, typeCheck((f, uf), tyEnv))
        ty
      case Rec(f, ft, x, xt, b) =>
        //validType(ft, tyEnv)
        //validType(xt, tyEnv)
        val newTyEnv = tyEnv + (f -> ft) + (x -> xt)
        uf.union(ft, ArrowT(xt, typeCheck((b, uf), newTyEnv)))
        ft
    }
  }

  def interpret(expr: Expr, env: Env): Value = ???

  def tests: Unit = {
    def testTypeCheck(str: String, output: String): Unit = {
      val (expr, uf) = TIRCFAE(str)
      test(typeCheck((expr, uf), Map()).toStringWith(uf), output)
    }

    def testExcTypeCheck(str: String, msg: String): Unit = {
      val (expr, uf) = TIRCFAE(str)
      testExc(typeCheck((expr, uf), Map()).toStringWith(uf), msg)
    }
    
    testTypeCheck("{fun {x : ?} {+ x 1}}", "(num -> num)")
    testTypeCheck("{fun {y : ?} y}", "(? -> ?)")
    testTypeCheck("{{fun {y : ?} y} {fun {x : ?} {+ x 1}}}", "(num -> num)")
    testTypeCheck("{fun {y : ?} {y 7}}", "((num -> ?) -> ?)")
    testExcTypeCheck("{fun {x : ?} {x x}}", "cyclic type")
  }
}
