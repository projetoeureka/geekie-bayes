package geekie.math

import geekie.pgm.{NodesObservations, FreqTable}

object Discrete {

  def mutualInfo[N](jointFreqTable: FreqTable[N]): Double = {
    assert(jointFreqTable.nodes.length == 2, "Mutual information is only supported on two variables")

    val List(nA, nB) = jointFreqTable.nodes

    val margA = jointFreqTable.marginalize(nB)
    val margB = jointFreqTable.marginalize(nA)

    NodesObservations.allPossibleObservations(List(nA, nB)) map {
      case obs => {
        val jointProb = jointFreqTable.getCounts(obs).toDouble/jointFreqTable.counts.sum
        val aProb = margA.getCounts(obs - nB).toDouble/margA.counts.sum
        val bProb = margB.getCounts(obs - nA).toDouble/margB.counts.sum
        jointProb*Math.log(jointProb/(aProb*bProb))
      }
    } sum
  }

}
