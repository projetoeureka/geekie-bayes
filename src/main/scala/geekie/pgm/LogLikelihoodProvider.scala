package geekie.pgm


/**
 * LogLikelihoodProvider provides (duh) a fast way of accessing all possible likelihoods
 * for a node. A lazy a Map (node -> (parents -> log-likelihood)) is used to achieve that.
 *
 * @param modelProvider
 * @param sampleFreqTables
 * @tparam N
 */
case class LogLikelihoodProvider[N](modelProvider: ModelProvider[N],
                                    sampleFreqTables: List[FreqTable[N]]) {

  // Lazily calculate all possible likelihoods for a node
  lazy val logLikelihoodByNodeByParents = allLikelihoodsByNodeByParents

  lazy val sampleFreqTablesByNodes = sampleFreqTables map { case f => f.nodes.toSet -> f } toMap

  def logLikelihoodByParents(node: N): Map[Set[N], Double] =
    logLikelihoodByNodeByParents(node)

  def logLikelihood(parentsByNode: Map[N, Set[N]]): Double =
    parentsByNode map { case (node, parents) => logLikelihoodByNodeByParents(node)(parents) } sum

  /**
   * Returns the log logLikelihood of node `node` having parents `parents`, by
   * multiplying the counts of each possible combination of `node + parents` in sample
   * by the log of the probability value of that combination from `modelProvider`
   *
   * @param node
   * @param parents
   * @return
   */
  private def logLikelihood(node: N, parents: Set[N]): Double = {
    val modelCPD = modelProvider.CPDforParents(node, parents)

    NodesObservations.allPossibleObservations((parents + node).toList) map {
      case obs =>
        val countInSample = sampleFreqTablesByNodes(parents + node).getObservationCounts(obs)
        val modelProb = modelCPD.condProb(obs(node), obs - node)
        countInSample * Math.log(modelProb)
    } sum
  }

  private def allLikelihoodsByParents(node: N): Map[Set[N], Double] = {
    (for {
      parents <- sampleFreqTables.map(_.nodes.toSet).filter(_.contains(node)).map(_ - node)
    } yield parents -> logLikelihood(node, parents)).toMap
  }

  private def allLikelihoodsByNodeByParents: Map[N, Map[Set[N], Double]] = {
    val allNodes = sampleFreqTablesByNodes.keySet.reduce(_ ++ _)
    (for {
      (node, i) <- allNodes zipWithIndex
    } yield node -> allLikelihoodsByParents(node)).toMap
  }
}

