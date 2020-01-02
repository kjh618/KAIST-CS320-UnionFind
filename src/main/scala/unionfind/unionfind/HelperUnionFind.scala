package unionfind.unionfind

import scala.util.parsing.combinator._

trait HelperUnionFind extends unionfind.Helper {
  case class UnionFind(var parent: Map[Type, Type] = Map(), var varTNum: Int = 0) {
    def newVarTNum(): Int = {
      varTNum += 1
      varTNum
    }

    def makeSet(t: Type): Unit =
      if (!parent.contains(t)) parent = parent + (t -> t)
      else ()

    def find(t: Type): Type = {
      if (t == parent(t)) t
      else find(parent(t))
    }

    def union(t1: Type, t2: Type): Unit = {
      def occurs(vtRoot: VarT, t2: Type): Boolean = t2 match {
        case NumT => false
        case ArrowT(p, r) => occurs(vtRoot, p) || occurs(vtRoot, r)
        case VarT(_) =>
          val t2Root = find(t2)
          t2Root match {
            case VarT(_) => vtRoot == t2Root
            case _ => occurs(vtRoot, t2Root)
          }
      }

      def updateParent(vtRoot: VarT, newParent: Type): Unit =
        if (occurs(vtRoot, newParent)) error("cyclic type")
        else parent += (vtRoot -> newParent)

      makeSet(t1)
      makeSet(t2)

      val t1Root = find(t1)
      val t2Root = find(t2)
      (t1Root, t2Root) match {
        case (_, vt@VarT(_)) => updateParent(vt, t1Root)
        case (vt@VarT(_), _) => updateParent(vt, t2Root)
        case (ArrowT(p1, r1), ArrowT(p2, r2)) => union(p1, p2); union(r1, r2)
        case _ => mustSame(t1Root, t2Root)
      }
    }
  }

  trait Expr
  case class Num(num: Int) extends Expr
  case class Add(left: Expr, right: Expr) extends Expr
  case class Sub(left: Expr, right: Expr) extends Expr
  case class Id(name: String) extends Expr
  case class Fun(paramName: String, paramType: Type, body: Expr) extends Expr
  case class App(func: Expr, arg: Expr) extends Expr
  case class If0(cond: Expr, thenE: Expr, elseE: Expr) extends Expr
  case class Rec(funcName: String, funcType: Type, paramName: String, paramType: Type, body: Expr) extends Expr

  def notype(msg: Any): Nothing = error(s"no type: $msg")

  def same(left: Type, right: Type): Boolean = (left, right) match {
    case (NumT, NumT) => true
    case (ArrowT(p1, r1), ArrowT(p2, r2)) =>
      same(p1, p2) && same(r1, r2)
    case _ => false
  }

  def mustSame(left: Type, right: Type): Type =
    if (same(left, right)) left
    else notype(s"$left is not equal to $right")

  trait Type {
    def toStringWith(uf: UnionFind): String = this match {
      case NumT => "num"
      case ArrowT(p, r) => s"(${p.toStringWith(uf)} -> ${r.toStringWith(uf)})"
      case VarT(_) => uf.find(this) match {
        case VarT(_) => "?"
        case t => t.toStringWith(uf)
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

  type Env = Map[String, Value]
  type TypeEnv = Map[String, Type]

  def typeCheck(tuple: (Expr, UnionFind), typeEnv: TypeEnv): Type

  def interpret(expr: Expr, env: Env): Value

  def run(str: String): String = {
    val (expr, uf) = TIRCFAE(str)
    typeCheck((expr, uf), Map())
    interpret(expr, Map()).toString
  }

  object TIRCFAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"

    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)

    lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r

    lazy val expr: Parser[Expr] =
      int ^^ { n => Num(n) } |
      wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
      wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
      str ^^ { x => Id(x) } |
      wrap("fun" ~> wrap(str ~ (":" ~> ty)) ~ expr) ^^ { case pn ~ pt ~ b => Fun(pn, pt, b) } |
      wrap(expr ~ expr) ^^ { case f ~ a => App(f, a) } |
      wrap("if0" ~> expr ~ expr ~ expr) ^^ { case c ~ t ~ e => If0(c, t, e) } |
      wrap("recfun" ~> wrap((str ~ (":" ~> ty)) ~ (str ~ (":" ~> ty))) ~ expr) ^^
        { case (fn ~ ft) ~ (pn ~ pt) ~ b => Rec(fn, ft, pn, pt, b)}

    val uf = UnionFind()

    lazy val ty: Parser[Type] =
      "num" ^^ { _ => uf.makeSet(NumT); NumT } |
      "(" ~> ((ty <~ "->") ~ ty) <~ ")" ^^ { case p ~ r => uf.makeSet(ArrowT(p, r)); ArrowT(p, r) } |
      "?" ^^ { _ =>
        val num = uf.newVarTNum()
        uf.makeSet(VarT(num)); VarT(num)
      }

    def apply(str: String): (Expr, UnionFind) = {
      val result = parseAll(expr, str).getOrElse(error(s"bad syntax: $str"))
      (result, uf)
    }
  }
}
