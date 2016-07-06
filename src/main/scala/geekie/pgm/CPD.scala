package geekie.pgm

import geekie.utils.BinaryIndexDecoder

/**
 * Represents a Conditional Probability Distribution
 *
 * @param parents List of nodes on which this distribution depends on. E.g. for P(a | b,c), parents = List(b, c)
 * @param probabilities List of probabilities in order. E.g.: FFF, FFT, FTF, FTT, TFF, ... , TTT
 * @tparam N Node type
 */
case class CPD[N](parents: List[N], probabilities: List[Double]) {

  //TODO: raise exception for malformed input
  assert(probabilities.length == (1 << parents.length),
    f"Number of parent nodes (${parents.length}) is incompatible with number of PDF coefficients (${probabilities.length})")

  private val rnd = scala.util.Random

  def draw(parentsObservations: NodesObservations[N]) = {
    rnd.nextDouble() < condProb(nodeObservation = true, parentsObservations)
  }

  def condProb(nodeObservation: Boolean, parentsObservations: NodesObservations[N]) = {
    val theta = probabilities(probIndex(parentsObservations))
    if (nodeObservation) theta else 1 - theta
  }

  private def probIndex(condState: NodesObservations[N]) = BinaryIndexDecoder(parents map (condState(_)))
}
