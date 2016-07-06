package geekie.pgm

import geekie.utils.BooleanCombinations

import scala.language.postfixOps


case class BayesNet[N](CPDbyNode: Map[N, CPD[N]]) {

  lazy val orderedNodes = topSort(CPDbyNode mapValues (_.parents.toSet), List())

  lazy val dag = dagFromCPD()

  private def dagFromCPD() = {
    val nodes = CPDbyNode.keys.toSet
    val edges = for {
      (node, cpd) <- CPDbyNode
      parent <- cpd.parents
    } yield Edge(node, parent)
    edges.foldLeft(DAG(nodes))((currentDag, edge) => currentDag.append(edge))
  }

  def topSort(deps: Map[N, Set[N]], done: List[N]): List[N] = {
    val (noParents, haveParents) = deps.partition { case (node, parents) => parents.isEmpty }
    if (noParents.isEmpty)
      if (haveParents.isEmpty) done else throw cycleException("The network has a cycle!")
    else {
      val found = noParents.keySet
      topSort(haveParents.mapValues(_ -- found), done ++ found)
    }
  }

  def drawObservation() = (NodesObservations[N]() /: orderedNodes) {
    (observations, node) =>
      observations + (node -> CPDbyNode(node).draw(observations))
  }

  def jointProb(observation: NodesObservations[N]) = {
    CPDbyNode map {
      case (node, cpd) => cpd.condProb(observation(node), observation)
    } product
  }

  def +(b: BayesNet[N]) = BayesNet[N](CPDbyNode ++ b.CPDbyNode)

  def serializable = CPDbyNode.toList
}
