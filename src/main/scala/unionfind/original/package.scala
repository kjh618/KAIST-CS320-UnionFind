package unionfind

package object original extends HelperOriginal {
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

  def resolve(ty: Type): Type = ty match {
    case VarT(None) => ty
    case VarT(Some(t)) => resolve(t)
    case _ => ty
  }

  def occurs(t1: VarT, t2: Type): Boolean = t2 match {
    case NumT => false
    case ArrowT(l, r) => occurs(t1, l) || occurs(t1, r)
    case VarT(ty) => (t1 eq t2) || (ty match {
      case None => false
      case Some(t) => occurs(t1, t)
    })
  }

  def unify(t1: Type, t2: Type): Unit = t1 match {
    case vt@VarT(ty) => ty match {
      case Some(t) => unify(t, t2)
      case _ =>
        val t = resolve(t2)
        if (vt eq t) ()
        else {
          if (occurs(vt, t)) error(s"cyclic type: $t")
          else vt.ty = Some(t)
        }
    }
    case _ => t2 match {
      case VarT(ty) => unify(t2, t1)
      case NumT => mustSame(t1, t2); ()
      case ArrowT(l, r) => t1 match {
        case ArrowT(a, b) =>
          unify(l, a)
          unify(r, b)
        case _ =>
          error(s"not an arrow type: $t1")
      }
    }
  }

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

  def tests: Unit = {
    test(typeCheck(TIRCFAE("{fun {x : ?} {+ x 1}}"), Map()).toString, "(num -> num)")
    test(typeCheck(TIRCFAE("{fun {y : ?} y}"), Map()).toString, "(? -> ?)")
    test(typeCheck(TIRCFAE("{{fun {y : ?} y} {fun {x : ?} {+ x 1}}}"), Map()).toString, "(num -> num)")
    test(typeCheck(TIRCFAE("{fun {y : ?} {y 7}}"), Map()).toString, "((num -> ?) -> ?)")
    testExc(typeCheck(TIRCFAE("{fun {x : ?} {x x}}"), Map()).toString, "cyclic type")
  }
}
