package geekie.pgm.inference

import geekie.pgm.{BayesNet, NodesObservations}
import geekie.utils.BooleanCombinations

/**
 * Inference by enumeration. It's the most simple general inference algorithm there is.
 * It simply "uneducatedly" enumerates all possible combinations of hidden variables
 * (non-query, non-evidence) and sums their joint probability.
 *
 * @param bayesNet
 * @tparam N
 */
case class Enumeration[N <% Ordered[N]](private val bayesNet: BayesNet[N]) extends InferenceAlgorithm[N] {

  override def infer(query: Set[N], evidence: NodesObservations[N]): Factor[N] = {

    if (query.size != 1) throw new UnsupportedOperationException("Enumeration inference is so far only implemented for one query variable!")

    val hiddenVariables = (bayesNet.CPDbyNode.keySet -- evidence.keySet -- query).toSeq

    val unNormalizedProbTrue = sumOutHiddenVariables(evidence + (query.head -> true), hiddenVariables)
    val unNormalizedProbFalse = sumOutHiddenVariables(evidence + (query.head -> false), hiddenVariables)

    Factor(Scope(query), List(unNormalizedProbFalse, unNormalizedProbTrue)).normalize
  }

  private def sumOutHiddenVariables(evidence: NodesObservations[N], hiddenVariables: Seq[N]): Double = {
    BooleanCombinations(hiddenVariables.size) map {
      values => bayesNet.jointProb(evidence ++ NodesObservations(hiddenVariables.zip(values).toMap))
    } sum
  }
}
