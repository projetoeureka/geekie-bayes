package geekie.pgm

import geekie.utils.BinaryIndexDecoder

/**
 * `ModelProvider` is a helper class that produces the optimal
 * parameters (CPDs) for a given topology. It does so by applying a
 * MAP estimator, in which, after derivation, the probability of, for instance,
 * P(A=T|B=T) is given by  P(A=T|B=T)/(P(A=T|B=T) + P(A=F|B=T)). That is, its frequency.
 *
 * @param dataFreqTables: The "counts" output.
 * @tparam N
 */
case class ModelProvider[N](dataFreqTables: Iterator[FreqTable[N]]) {

  lazy val scope = dataFreqTablesByNodes.keysIterator.reduce(_ ++ _)

  // TODO: Handle multiple values for the same key (maybe just reduce them?)
  private lazy val dataFreqTablesByNodes = dataFreqTables map { case f => (f.nodes.toSet -> f) } toMap

  /**
   * Returns a single CPD for a given a node `node` and its parents `parents`
   * @param node
   * @param parents
   * @return
   */
  def CPDforParents(node: N, parents: Set[N]): CPD[N] = {
    dataFreqTablesByNodes(parents + node).getCPD(node)
  }

  def CPDforParents(parentsByNode: Map[N, Set[N]]): Map[N, CPD[N]] = {
    parentsByNode map {
      case (node, parents) => node -> CPDforParents(node, parents)
    }
  }
}
