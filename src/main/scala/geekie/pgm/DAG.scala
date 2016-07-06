package geekie.pgm

case class cycleException(es: String) extends Exception(es)


case class nodeNotFoundException(es: String) extends Exception(es)


case class Edge[N](a: N, b: N) {
  def t = Edge(b, a)

  override def toString = f"$a -> $b"
}

object DAG {
  def apply[N](nodes: Set[N]) = new DAG[N](nodes, Set(), Vector())
}


/** A DAG with a list of all the paths, that allows you to find out the successors and ancestors from each node, and to
  * easily check for cycles -- and for paths.
  *
  * @param nodes a `Vector` with the nodes present in the graph.
  * @param edges a `Vector` of Edges
  * @param paths a list of all paths, calculated via `append` or `remove`.
  */
class DAG[N](val nodes: Set[N], val edges: Set[Edge[N]], private val paths: Vector[Edge[N]]) {

  def append(ed: Edge[N]) = {
    if ((ed.a == ed.b) || (paths contains ed.t)) throw cycleException("This edge would close a cycle in the DAG")
    if (!((nodes contains ed.a) && (nodes contains ed.b))) throw nodeNotFoundException("Attempt to insert an edge from/to a nonexistent node")
    if (edges contains ed) this
    else new DAG(nodes, edges + ed, paths ++ findPaths(ed))
  }

  def canAppend(ed: Edge[N]): Boolean = !((paths contains ed.t) || (edges contains ed) || (ed.a == ed.b))

  def remove(ed: Edge[N]) =
    if (edges contains ed) new DAG(nodes, edges - ed, paths diff findPaths(ed))
    else this

  /** @param ab input edge.
    * @return list of paths that (might) contain the given edge.
    */
  def findPaths(ab: Edge[N]) = for (o <- origins(ab.a); d <- destinies(ab.b)) yield Edge(o, d)

  /** @param from input node
    * @return list of nodes with paths to the given edge
    */
  def origins(from: N) = from +: (paths filter (_.b == from) map (_.a))

  /** @param from input node
    * @return list of nodes with paths starting from the given node
    */
  def destinies(from: N) = from +: (paths filter (_.a == from) map (_.b))

  def edgesFromNode(node: N): Set[Edge[N]] = this.edges filter { edge => edge.a == node }

  def edgesToNode(node: N): Set[Edge[N]] = this.edges filter { edge => edge.b == node }

  def parents(node: N): Set[N] = edgesFromNode(node) map (_.b)

  def children(node: N): Set[N] = edgesToNode(node) map (_.a)

  override def toString = edges.toList.sortBy(_.b.toString).sortBy(_.a.toString).toString()

  override def equals(other: Any): Boolean = other match {
    case b: DAG[N] => (nodes == b.nodes) && (edges == b.edges)
    case _ => false
  }

  override def hashCode = (nodes, edges).hashCode
}
