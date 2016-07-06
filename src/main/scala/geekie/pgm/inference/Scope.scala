package geekie.pgm.inference

object Scope {
  def apply[N <% Ordered[N]](): Scope[N] = Scope[N](Set())
}

/**
 *  The `Scope` class garantees the iteration order on its variables.
 *  It can be instantiates from any Traversable.
 *
 * @param nodes the variables that will compose the scope
 * @tparam N nodes' type
 */
case class Scope[N <% Ordered[N]](private val nodes: Traversable[N]) extends Iterable[N] {

  private lazy val nodeSet: Set[N] = nodes.toSet

  override def iterator: Iterator[N] = nodeSet.toList.sorted.iterator

  def +(other: N) = Scope(nodes ++ Traversable(other))

  def ++(other: Scope[N]) = Scope(nodes ++ other)

  def -(other: N) = Scope(nodes.filter(_ != other))

  def --(other: Scope[N]) = Scope(nodes.toSet diff other.toSet)

  def contains(which: N): Boolean = nodeSet.contains(which)
}
