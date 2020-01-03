package unionfind.unionfind

case class TypeUnionFind(var parent: Map[Type, Type] = Map(), var varTNum: Int = 0) {
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
