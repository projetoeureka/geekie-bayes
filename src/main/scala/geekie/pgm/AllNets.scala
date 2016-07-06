package geekie.pgm

object AllNets {
  def allNets[N](allNodes: List[N], maxParents: Int) = {
    val totalNodes = allNodes.length
    val totalStates = totalNodes * totalNodes

    def recursiveNetGenerator(currentState: DAG[N], currentIndex: Int): Stream[DAG[N]] = {
      if (currentIndex == totalStates) Stream(currentState)
      else {
        if (currentState.canAppend(currentEdge(currentIndex)) && canHaveMoreParents(currentState, currentEdge(currentIndex)))
          recursiveNetGenerator(currentState, currentIndex + 1) #::: recursiveNetGenerator(currentState.append(currentEdge(currentIndex)), currentIndex + 1)
        else
          recursiveNetGenerator(currentState, currentIndex + 1)
      }
    }

    def currentEdge(currentIndex: Int): Edge[N] = {
      val row = currentIndex / totalNodes
      val col = currentIndex % totalNodes
      Edge(allNodes(row), allNodes(col))
    }

    def canHaveMoreParents(dag: DAG[N], newEdge: Edge[N]): Boolean =
      dag.edges.count(e => e.a == newEdge.a) < maxParents


    recursiveNetGenerator(DAG(allNodes.toSet), 0)
  }
}
