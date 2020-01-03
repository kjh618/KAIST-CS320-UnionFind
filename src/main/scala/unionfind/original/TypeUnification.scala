package unionfind.original

object TypeUnification {
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

  def unify(t1: Type, t2: Type): Unit = {
    unifyOrignal(t1, t2)
    //unifyModified(t1, t2)
  }

  def unifyOrignal(t1: Type, t2: Type): Unit = t1 match {
    case vt@VarT(ty) => ty match {
      case Some(t) => unifyOrignal(t, t2)
      case _ =>
        val t = resolve(t2)
        if (vt eq t) ()
        else {
          if (occurs(vt, t)) error(s"cyclic type: $t")
          else vt.ty = Some(t)
        }
    }
    case _ => t2 match {
      case VarT(ty) => unifyOrignal(t2, t1)
      case NumT => mustSame(t1, t2); ()
      case ArrowT(l, r) => t1 match {
        case ArrowT(a, b) =>
          unifyOrignal(l, a)
          unifyOrignal(r, b)
        case _ =>
          error(s"not an arrow type: $t1")
      }
    }
  }

  def unifyModified(t1: Type, t2: Type): Unit = {
    def updateParent(vtRoot: VarT, newParent: Type): Unit =
      if (occurs(vtRoot, newParent)) error("cyclic type")
      else vtRoot.ty = Some(newParent)

    val t1Root = resolve(t1)
    val t2Root = resolve(t2)
    (t1Root, t2Root) match {
      case (_, vt@VarT(None)) => updateParent(vt, t1Root)
      case (vt@VarT(None), _) => updateParent(vt, t2Root)
      case (ArrowT(p1, r1), ArrowT(p2, r2)) => unifyModified(p1, p2); unifyModified(r1, r2)
      case _ => mustSame(t1Root, t2Root)
    }
  }
}
