package geekie.pgm

import geekie.pgm.inference.InferenceAlgorithm
import geekie.utils.BooleanCombinations


/**
 * This an experimental implementation of a very weird dynamic Bayesian network.
 *
 * @param bayesNet
 * @param priors
 * @param priorFactory
 * @param transitionModelFactory
 * @param sensorModelFactory
 */
case class WeirdDBN[N <%  Ordered[N]](bayesNet: BayesNet[N],
                                 priors: Map[N, Double],
                                 priorFactory: (N, Map[N, Double]) => (N, CPD[N]),
                                 transitionModelFactory: N => CPD[N],
                                 sensorModelFactory: N => (N, CPD[N])) {

  // Rolled out full-blown net
  lazy val unrolledNet = {

    val originalCpdByNodes = bayesNet.CPDbyNode

    def mergeParent(node: N, origCpd: CPD[N], newParentCPD: CPD[N]): CPD[N] = {

      val newParents = newParentCPD.parents.union(origCpd.parents)

      val newProbValues = NodesObservations.allPossibleObservations(newParents).map {
        case parentsObs => {

          // Un-normalized probability tuple < P(node=F | parentsObs), P(node=T | parentsObs) >
          val unorm: List[Double] = BooleanCombinations(1) map {
            case List(nodeObs) =>
              val origParentsObs = NodesObservations(parentsObs.filterKeys(origCpd.parents.contains))
              val newParentObs = NodesObservations(parentsObs.filterKeys(newParentCPD.parents.contains))
              val origProb = if(origCpd.parents.isEmpty) 1.0 else origCpd.condProb(nodeObs, origParentsObs)
              val newParentProb = newParentCPD.condProb(nodeObs, newParentObs)
              origProb * newParentProb
          }
          unorm(1)/unorm.sum
        }
      }
      CPD(newParents, newProbValues.toList)
    }

    // Attach transition models
    val cpdByNodeWithTransition = originalCpdByNodes map {
      case (node, originalCPD) => {
        node -> mergeParent(node, originalCPD, transitionModelFactory(node))
      }
    }

    // Attach priors
    val CPDByNodeWithPriors = cpdByNodeWithTransition.flatMap {
      case (node, cpd) =>
        val (newNode, newCPD) = priorFactory(node, priors)
        List(node -> cpd, newNode -> newCPD)
    }

    // Attach sensor to the every node
    val CPDByNodeWithSensors = bayesNet.CPDbyNode.foldLeft(CPDByNodeWithPriors) {
      case (acc, (node, cpd)) => {
        val (sensorNode, sensorCPD) = sensorModelFactory(node)
        acc.updated(sensorNode, sensorCPD)
      }
    }

    BayesNet(CPDByNodeWithSensors)
  }

  // Receives an evidence and propagate network at the next time point
  def step(evidenceNode: N,
           evidenceValue: Boolean,
           inferFactory: (BayesNet[N]) => InferenceAlgorithm[N]): WeirdDBN[N] = {

    val (sensorNode, _) = sensorModelFactory(evidenceNode)

    val infer = inferFactory(unrolledNet)

    val newPriorsByNode = bayesNet.CPDbyNode map {
      case (node, _) =>
        node -> infer.infer(node, NodesObservations() + (sensorNode, evidenceValue))(NodesObservations() + (node, true))
    }

    WeirdDBN(bayesNet, newPriorsByNode, priorFactory, transitionModelFactory, sensorModelFactory)
  }
}
