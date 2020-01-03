package unionfind.explicit

trait Expr
case class Num(num: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Id(name: String) extends Expr
case class Fun(paramName: String, paramType: Type, body: Expr) extends Expr
case class App(func: Expr, arg: Expr) extends Expr
case class If0(cond: Expr, thenE: Expr, elseE: Expr) extends Expr
case class Rec(funcName: String, funcType: Type, paramName: String, paramType: Type, body: Expr) extends Expr

trait Type {
  def toStringWith(tuf: TypeUnionFind): String = this match {
    case NumT => "num"
    case ArrowT(p, r) => s"(${p.toStringWith(tuf)} -> ${r.toStringWith(tuf)})"
    case VarT(_) => tuf.find(this) match {
      case VarT(_) => "?"
      case t => t.toStringWith(tuf)
    }
  }
}
case object NumT extends Type
case class ArrowT(param: Type, result: Type) extends Type
case class VarT(num: Int) extends Type

trait Value {
  override def toString: String = this match {
    case NumV(n) => n.toString
    case CloV(_, _, _) => "<function>"
  }
}
case class NumV(num: Int) extends Value
case class CloV(param: String, body: Expr, var env: Env) extends Value
