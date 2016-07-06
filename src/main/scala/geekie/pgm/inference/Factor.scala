package geekie.pgm.inference

import geekie.pgm.{NodesObservations, CPD}
import geekie.utils.BooleanCombinations


object Factor {
  def apply[N <% Ordered[N]](node: N, cpd: CPD[N]): Factor[N] = {
    val scope: Scope[N] = Scope(cpd.parents.toSet + node)

    def value(combinations: List[Boolean]): Double = {
      val attributionByNode = scope.zip(combinations).toMap
      cpd.condProb(attributionByNode(node), NodesObservations(attributionByNode - node))
    }

    //Reverse boolean combinations, since least significant bit should come first
    val values: List[Double] = BooleanCombinations.rev(scope.size).map(value)

    Factor(scope, values)
  }

  def apply[N <% Ordered[N]](node: N, values: List[Double]): Factor[N] =
    Factor(Scope(Set(node)), values)
}


case class Factor[N <% Ordered[N]](scope: Scope[N], values: List[Double]) {
  val indexByNode: Map[N, Int] = scope.zipWithIndex.toMap

  // Returns the n-dimensional coordinates for the given nodes observations
  def coords(nodesObservations: NodesObservations[N]): List[Int] = {
    if (nodesObservations.keys != scope.toSet) throw new IllegalArgumentException(
      s"You need to supply exactly all observations - no more, no less! Scope: ${scope}, obs: ${nodesObservations}")
    scope.foldLeft(List[Int]()) { (coords, n) => coords :+ (if (nodesObservations(n)) 1 else 0) }
  }

  /**
   * Returns the CPD entry corresponding to the given nodes observations
   * Note: This is _not_ the joint probability. Specially since during the sumout
   * process, elements can even be larger than 1.0
   *
   * @param nodesObservations the instantiation of the nodes for which the probability will be returned
   * @return the corresponding CPD entry for `nodesObservations`
   */
  def apply(nodesObservations: NodesObservations[N]): Double = {
    val index = coords(nodesObservations).zipWithIndex.foldLeft(0) {
      case (acc, (coord, obsIndex)) => acc + (coord << obsIndex)
    }
    values(index)
  }


  /**
   * Adds the 2 (n-1)-dimensional matrix resulting of fixing `which`
   * to `true` _and_ `false` _if_ `evidence` does not contain `which`.
   * If `which` is part of the evidence, then `which` is fixed to that value.
   *
   * @param which The variable to be summed-out
   * @param evidence The evidence to be taken into account when summing out `which`
   * @return new Factor with `which` summed-out
   */
  def sumout(which: N, evidence: NodesObservations[N]): Factor[N] = {
    if (!scope.contains(which)) throw new IllegalArgumentException(s"Cannot sumout a node (${which}) that is not in factor's scope!")

    val newScope = scope - which

    val values = BooleanCombinations.rev(newScope.size).map {
      comb => {
        val relevantEvidence = NodesObservations(evidence.filter { case (node, _) => scope.contains(node) })
        val obs = NodesObservations(newScope.zip(comb).toMap) ++ relevantEvidence

        if (obs.keySet.contains(which))
          this(obs)
        else
          this(obs + (which -> false)) + this(obs + (which -> true))
      }
    }
    Factor(newScope, values)
  }

  // A shortcut for `sumout` without evidence
  def sumout(which: N): Factor[N] = sumout(which, NodesObservations())

  // `*` point-wise multiplicates `this` with `other`
  def *(other: Factor[N]): Factor[N] = {
    val newScope = other.scope ++ scope

    val values = BooleanCombinations.rev(newScope.size).map {
      comb => {
        val obs = NodesObservations(newScope.zip(comb).toMap)
        val thisObs = NodesObservations(obs.filter { case (node, ob) => scope.contains(node) })
        val otherObs = NodesObservations(obs.filter { case (node, ob) => other.scope.contains(node) })
        this(thisObs) * other(otherObs)
      }
    }
    Factor(newScope, values)
  }

  def normalize: Factor[N] = Factor(scope, values.map(_/values.sum))
}

