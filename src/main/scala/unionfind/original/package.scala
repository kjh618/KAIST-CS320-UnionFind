package unionfind

package object original extends HelperOriginal {
  def typeCheck(expr: Expr, typeEnv: TypeEnv): Type = ???

  def interpret(expr: Expr, env: Env): Value = ???

  def tests: Unit = {
    test("", "")
  }
}
