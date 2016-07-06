package geekie.ga

import geekie.pgm.LogLikelihoodProvider


case class TopSortIndividual[N](val topologicalOrder: List[N],
                           val logLikelihoodProvider: LogLikelihoodProvider[N],
                           val mutationRate: Double,
                           val crossoverRate: Double,
                           val maxParents: Int) extends Individual[TopSortIndividual[N]] {

  val rnd = new scala.util.Random(System.currentTimeMillis)

  private lazy val optimalTopology: Map[N, Set[N]] = makeOptimalTopology

  // For a given topological order, the fitness is equal to the log likelihood
  // of the network produced when every node select its best up to `maxParents` parents
  override lazy val fitness: Double = logLikelihoodProvider.logLikelihood(optimalTopology)

  override def crossover(mate: TopSortIndividual[N]): TopSortIndividual[N] = {
    //keep the common nodes, draw the rest
    val pairedGenomes = this.topologicalOrder zip mate.topologicalOrder
    val nonMatching = for ((a, b) <- pairedGenomes if a != b) yield a

    val childOrder = if (nonMatching.isEmpty)
      rnd.shuffle(this.topologicalOrder) //if both individuals are the same, create a new random one
    else {
      val shuffledNonMatching = rnd.shuffle(nonMatching)
      ((List[N](), shuffledNonMatching) /: pairedGenomes)({
        case ((acc, nn +: ns), (ga, gb)) => if (ga == gb) (acc :+ ga, nn +: ns) else (acc :+ nn, ns)
        case ((acc, Nil), (ga, gb)) => (acc :+ ga, Nil)
      })._1
    }

    if (rnd.nextDouble() < crossoverRate)
      TopSortIndividual(childOrder, logLikelihoodProvider, mutationRate, crossoverRate, maxParents)
    else this
  }

  override def mutate: TopSortIndividual[N] = {
    import geekie.utils.ListSwapMethod._

    val mutatedTopologicalOrder: List[N] = {
      if (rnd.nextDouble() < mutationRate) {
        //swap two nodes in the topological order
        val indexA = rnd.nextInt(topologicalOrder.length)
        val indexB = rnd.nextInt(topologicalOrder.length)

        topologicalOrder.swap(indexA, indexB)
      }
      else topologicalOrder
    }

    TopSortIndividual(mutatedTopologicalOrder, logLikelihoodProvider, mutationRate, crossoverRate, maxParents)
  }

  override def toString = f"$fitness $topologicalOrder"

  // Returns the `nParents` parents among `allowedParents` that minimizes the likelihood.
  private def bestParents(node: N, allowedParents: Set[N], maxParents: Int): Set[N] = {
    logLikelihoodProvider.logLikelihoodByParents(node).filterKeys {
      case (parents) => parents.subsetOf(allowedParents) && parents.size <= maxParents
    } maxBy {
      case (parents, logLikelihood) => logLikelihood
    } match {
      case (parents, _) => parents
    }
  }

  // Returns the Map node -> parents that maximize the likelihood
  private def makeOptimalTopology: Map[N, Set[N]] = {
    val parentsByNode = (for {
      (node, index) <- topologicalOrder.zipWithIndex
      allowedParents = topologicalOrder.drop(index).toSet
    } yield {
        (node, bestParents(node, allowedParents, maxParents))
      }).toMap

    parentsByNode
  }
}


object TopSortIndividual {
  val rnd = new scala.util.Random(System.currentTimeMillis)

  def makeRandomIndividual[N](nodes: Set[N],
                              logLikelihoodProvider: LogLikelihoodProvider[N],
                              mutationRate: Double,
                              crossoverRate: Double,
                              maxParents: Int) = {

    new TopSortIndividual(rnd.shuffle(nodes.toList), logLikelihoodProvider, mutationRate, crossoverRate, maxParents)
  }
}
