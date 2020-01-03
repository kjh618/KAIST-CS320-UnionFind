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
}
