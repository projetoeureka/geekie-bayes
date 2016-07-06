package geekie.pgm.inference

import geekie.pgm.{CPD, NodesObservations, BayesNet}

/**
 * This inference algorithm is somewhat useless given that it only works for naïve bayesian
 * networks and that we have more general algorithms such as the variable elimination.
 * Nevertheless, it's still useful for benchmarking and for proving other less intuitive
 * algorithms correct, at least for naïve nets.
 *
 * @param bayesNet
 * @tparam N
 */
case class NaiveBayes[N <% Ordered[N]](private val bayesNet: BayesNet[N]) extends InferenceAlgorithm[N] {
  override def infer(query: Set[N], evidence: NodesObservations[N]): Factor[N] = {

    if (query.size != 1) throw new IllegalArgumentException("Naive bayes inference only works for one query variable!")

    val priori = Factor(query.head, bayesNet.CPDbyNode(query.head))
    val posteriori = evidence.foldLeft(priori)(updateEvidence)
    posteriori.normalize
  }

  private def updateEvidence(priori: Factor[N], obs: (N, Boolean)): Factor[N] = {
    val (node, _) = obs
    val factor = Factor(node, bayesNet.CPDbyNode(node)).sumout(node, NodesObservations() + obs)
    priori * factor
  }

}
