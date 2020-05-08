class BinarySyntaxTree[V](implicit ord: Ordering[V]) {
  type Key = V

  private sealed trait AbstractTree[Key]

  private case object EmptyNode extends AbstractTree[V]

  private case class Branch(k: V, left: AbstractTree[V], right: AbstractTree[V])
      extends AbstractTree[V]

  private var tree: AbstractTree[V] = EmptyNode

  private def put(key: V, t: AbstractTree[V]): AbstractTree[V] = t match {
    case EmptyNode                            => Branch(key, EmptyNode, EmptyNode)
    case Branch(k, l, r) if ord.equiv(k, key) => Branch(k, l, r)
    case Branch(k, l, r) if ord.lt(key, k) =>
      Branch(k, put(key, l), r)
    case Branch(k, l, r) => Branch(k, l, put(key, r))
  }

  private def depth(tree: AbstractTree[V]): Int = {
    tree match {
      case Branch(_, l, r) =>
        Math.max(depth(l), depth(r)) + 1
      case _ => 0
    }
  }
  @annotation.tailrec
  private def min(t: AbstractTree[V]): Option[V] = t match {
    case EmptyNode               => None
    case Branch(k, EmptyNode, _) => Some(k)
    case Branch(_, l, _)         => min(l)
  }
  @annotation.tailrec
  private def max(t: AbstractTree[V]): Option[V] = t match {
    case EmptyNode               => None
    case Branch(k, _, EmptyNode) => Some(k)
    case Branch(_, _, r)         => max(r)
  }

  def put(key: V): Unit =
    tree = put(key, tree)

  def depth: Int = depth(tree)

  def min: Option[V] = min(tree)

  def max: Option[V] = max(tree)
}

object BinarySyntaxTree {
  def apply[K](values: K*)(implicit ord: Ordering[K]): BinarySyntaxTree[K] = {
    val tree = new BinarySyntaxTree[K]
    values.foreach(tree.put)
    tree
  }
}
