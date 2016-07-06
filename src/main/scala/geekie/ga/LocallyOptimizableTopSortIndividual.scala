package geekie.ga

import geekie.localsearch.{LocalSearch, LocallyOptimizable}
import geekie.pgm.LogLikelihoodProvider
import geekie.utils.{NullProgressReport, ProgressReport}


// TODO: For the love of god, fix this inheritance scheme. Find a way of making a BaseTopSortIndividual.
case class LocallyOptimizableTopSortIndividual[N](
                                topologicalOrder: List[N],
                                logLikelihoodProvider: LogLikelihoodProvider[N],
                                mutationRate: Double,
                                crossoverRate: Double,
                                maxParents: Int)(implicit progressReporter: ProgressReport = NullProgressReport)
                                  extends Individual[LocallyOptimizableTopSortIndividual[N]]
                                  with LocallyOptimizable[LocallyOptimizableTopSortIndividual[N]] {

  def objective = fitness

  // TODO: does this neighborhood make sense? What about its size?
  def neighborhood = {
    val neighbors = (for {
      oldIndex <- topologicalOrder.indices
      newIndex <- topologicalOrder.indices if newIndex != oldIndex
      y = topologicalOrder.take(oldIndex) ++ topologicalOrder.drop(oldIndex + 1)
    } yield new LocallyOptimizableTopSortIndividual(
        (y.take(newIndex) :+ topologicalOrder(oldIndex)) ++ y.drop(newIndex),
        logLikelihoodProvider,
        mutationRate,
        crossoverRate,
        maxParents)).toList

    // Parallel fitness pre calc
    neighbors.par.foreach(_.fitness)

    neighbors
  }

  val rnd = new scala.util.Random(System.currentTimeMillis)

  private lazy val optimalTopology: Map[N, Set[N]] = makeOptimalTopology

  // For a given topological order, the fitness is equal to the log likelihood
  // of the network produced when every node select its best up to `maxParents` parents
  override lazy val fitness: Double = logLikelihoodProvider.logLikelihood(optimalTopology)

  override def crossover(mate: LocallyOptimizableTopSortIndividual[N]): LocallyOptimizableTopSortIndividual[N] = {
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

    if (rnd.nextDouble() < crossoverRate) {
      val originalChild = new LocallyOptimizableTopSortIndividual(childOrder, logLikelihoodProvider, mutationRate, crossoverRate, maxParents)

      // Find the localy best
      val localSearcher = new LocalSearch[LocallyOptimizableTopSortIndividual[N]]()
      localSearcher(originalChild)
    }
    else this
  }

  override def mutate: LocallyOptimizableTopSortIndividual[N] = {
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

    LocallyOptimizableTopSortIndividual(mutatedTopologicalOrder, logLikelihoodProvider, mutationRate, crossoverRate, maxParents)
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
  def makeOptimalTopology: Map[N, Set[N]] = {
    val parentsByNode = (for {
      (node, index) <- topologicalOrder.zipWithIndex
      allowedParents = topologicalOrder.drop(index).toSet
    } yield {
        (node, bestParents(node, allowedParents, maxParents))
      }).toMap

    parentsByNode
  }
}


object LocallyOptimizableTopSortIndividual {
  val rnd = new scala.util.Random(System.currentTimeMillis)

  def makeRandomIndividual[N](nodes: Set[N],
                              logLikelihoodProvider: LogLikelihoodProvider[N],
                              mutationRate: Double,
                              crossoverRate: Double,
                              maxParents: Int)
                             (implicit report: ProgressReport = NullProgressReport) = {

    new LocallyOptimizableTopSortIndividual(
      rnd.shuffle(nodes.toList),
      logLikelihoodProvider,
      mutationRate,
      crossoverRate,
      maxParents)
  }
}
