package unionfind.`implicit`

import scala.util.parsing.combinator._

trait HelperSpecific extends unionfind.Helper {
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

  type Env = Map[String, Value]
  type TypeEnv = Map[String, Type]

  def typeCheck(expr: Expr, typeEnv: TypeEnv): Type

  def interpret(expr: Expr, env: Env): Value

  def run(str: String): String

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
