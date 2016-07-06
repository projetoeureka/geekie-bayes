package geekie.pgm.inference

import geekie.pgm.{BayesNet, NodesObservations}

case class VariableElimination[N <% Ordered[N]](private val bayesNet: BayesNet[N]) extends InferenceAlgorithm[N] {

  lazy val factors = bayesNet.CPDbyNode map { case (node, cpd) => Factor(node, cpd) }

  /**
   * Returns the normalized `Factor` which scope contains only the query
   * variables and values contain their probability distribution.
   *
   * Specifically for the variable elimination algorithm, the inference process
   * consists of a series of multiplications between `Factors` and sumouts of variables
   * from these resulting `Factors`. Finding the optimal ordering of nodes to apply
   * these operators is a NP problem, so the strategy adopted here is simply multiplying
   * factors in the order they naturally appear and sumout variables as soon as they are
   * not seen in any other factor. This could probably be improved by a smarter heuristic.
   *
   * @param query
   * @param evidence
   * @return
   */
  def infer(query: Set[N], evidence: NodesObservations[N]): Factor[N] = {

    val factor = factors.tail.zipWithIndex.foldLeft(factors.head) {
      case (acc, (fac, position)) => {
        val res = acc * fac

        // Sumout non-query variables that are in `res` scope but not anywhere else
        (res.scope -- scopeAfter(fac) -- Scope(query)).foldLeft(res) {
          case (factor, node) => factor.sumout(node, evidence)
        }
      }
    }
    factor.normalize
  }

  /**
   * Returns a "joint scope" with nodes that are in all scopes _after_ `currentFactor`.
   *
   * @param currentFactor
   * @return
   */
  private def scopeAfter(currentFactor: Factor[N]): Scope[N] = {
    factors.dropWhile(_ != currentFactor).drop(1).foldLeft(Scope[N]()) {
      case (acc, factor) => acc ++ factor.scope
    }
  }
}
