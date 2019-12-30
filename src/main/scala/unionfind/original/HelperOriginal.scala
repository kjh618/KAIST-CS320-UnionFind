package unionfind

import scala.util.parsing.combinator._

trait HelperOriginal extends Helper {
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
    override def toString: String = this match {
      case NumT => "num"
      case ArrowT(p, r) => s"($p -> $r)"
      case VarT(None) => "?"
      case VarT(Some(t)) => t.toString
    }
  }
  case object NumT extends Type
  case class ArrowT(param: Type, result: Type) extends Type
  case class VarT(var ty: Option[Type]) extends Type

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

  def typeCheck(expr: Expr, typeEnv: TypeEnv): Type

  def interpret(expr: Expr, env: Env): Value

  def run(str: String): String = {
    val expr = TIRCFAE(str)
    typeCheck(expr, Map())
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

    lazy val ty: Parser[Type] =
      "num" ^^ { _ => NumT } |
      "(" ~> ((ty <~ "->") ~ ty) <~ ")" ^^ { case p ~ r => ArrowT(p, r) } |
      "?" ^^ { _ => VarT(None) }

    def apply(str: String): Expr = parseAll(expr, str).getOrElse(error(s"bad syntax: $str"))
  }
}
