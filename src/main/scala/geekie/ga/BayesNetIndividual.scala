package geekie.ga

import geekie.pgm._
import org.apache.commons.math3.stat.inference.TestUtils._

import scala.language.postfixOps


/** An individual representation of a DAG.
  *
  * @param bayesNet a `BayesNet` instance with accessible DAG
  * @param modelProvider a `CPDProvider` with all possible connection's counts
  * @param logLikelihoodProvider Map of log likelihood for different configurations of node -> parents
  *
  */
case class BayesNetIndividual[N](bayesNet: BayesNet[N],
                                 modelProvider: ModelProvider[N],
                                 logLikelihoodProvider: LogLikelihoodProvider[N],
                                 mutationRate: Double,
                                 crossoverRate: Double,
                                 maxParents: Int
                                  ) extends Individual[BayesNetIndividual[N]] {

  val rnd = scala.util.Random

  private def dag = bayesNet.dag

  private def nodes = bayesNet.dag.nodes

  private lazy val parentsByNode = bayesNet.CPDbyNode.map {
    case (node, cpd) => node -> cpd.parents.toSet
  }

  lazy val fitness = logLikelihoodProvider.logLikelihood(parentsByNode)

  def crossover(other: BayesNetIndividual[N]): BayesNetIndividual[N] = {
    lazy val childNodes = this.nodes.union(other.nodes)
    lazy val nodesOrder = rnd.shuffle(childNodes.toList)
    lazy val crossPoint = if (childNodes.nonEmpty) rnd.nextInt(childNodes.size) else 0

    def populateDAG(currentDAG: DAG[N], nodesQueue: List[N]): DAG[N] = {
      if (nodesQueue.isEmpty) currentDAG
      else {
        def appendEdges(currentDAG: DAG[N], origin: N) = {
          val parent = if (nodesQueue.length > crossPoint) this else other
          val newEdges = parent.dag.edgesFromNode(origin)

          newEdges.foldLeft(currentDAG)((dag, e) => if (dag.canAppend(e)) dag.append(e) else dag)
        }

        populateDAG(appendEdges(currentDAG, nodesQueue.head), nodesQueue.tail)
      }
    }
    lazy val childDAG = populateDAG(DAG(childNodes), nodesOrder)

    if (rnd.nextDouble() < crossoverRate)
      BayesNetIndividual(
        BayesNet(CPDByNodeForDAG(childDAG)),
        modelProvider,
        logLikelihoodProvider,
        mutationRate,
        crossoverRate,
        maxParents)

    else this
  }

  def mutate: BayesNetIndividual[N] = {

    def mutateDAG(currentDAG: DAG[N], origin: N) = {
      val edgesFromNode = dag.edgesFromNode(origin)
      val possibleDests = (nodes - origin).toList
      val newEdge = Edge(origin, possibleDests(rnd.nextInt(possibleDests.size)))
      val nParents = maxParents - rnd.nextInt(1 + rnd.nextInt(maxParents + 1))
      val newEdges = rnd.shuffle(edgesFromNode + newEdge).take(nParents)
      val strippedDAG = (currentDAG /: edgesFromNode)((dag, e) => dag.remove(e))

      (strippedDAG /: newEdges) {
        (dag, e) => if (dag.canAppend(e)) dag.append(e) else dag
      }
    }

    val mutatedDag = (dag /: nodes.filter(_ => rnd.nextDouble() < mutationRate)) {
      (currentDag, node) => mutateDAG(currentDag, node)
    }

    BayesNetIndividual(
      BayesNet(CPDByNodeForDAG(mutatedDag)),
      modelProvider,
      logLikelihoodProvider,
      mutationRate,
      crossoverRate,
      maxParents)
  }

  private def CPDByNodeForDAG(dag: DAG[N]): Map[N, CPD[N]] =
    parentsByNode map { case (node, parents) => node -> modelProvider.CPDforParents(node, parents) }

  override def toString = f"$fitness $dag"
}


object BayesNetIndividual {

  def makeRandomIndividualOld[N](allNodes: List[N],
                                 modelProvider: ModelProvider[N],
                                 logLikelihoodProvider: LogLikelihoodProvider[N],
                                 mutationRate: Double,
                                 crossoverRate: Double,
                                 maxParents: Int) = {
    val rnd = new scala.util.Random(System.currentTimeMillis)
    val totalNodes = allNodes.length
    val totalEdges = rnd.nextInt((totalNodes * (totalNodes - 1)) / 2)

    val newEdges = for (_ <- 1 to totalEdges) yield Edge(allNodes(rnd.nextInt(totalNodes)), allNodes(rnd.nextInt(totalNodes)))

    val dag = newEdges.foldLeft(DAG(allNodes.toSet))((dag, e) => {
      if (dag.canAppend(e) && (dag.edges.count(_.a == e.a) < maxParents)) dag.append(e) else dag
    })

    val CPDByNode = dag.nodes.map {
      case node => node -> modelProvider.CPDforParents(node, dag.parents(node))
    }.toMap

    BayesNetIndividual(
      BayesNet(CPDByNode),
      modelProvider,
      logLikelihoodProvider,
      mutationRate,
      crossoverRate,
      maxParents
    )
  }


  def makeRandomIndividual[N](allNodes: List[N],
                              modelProvider: ModelProvider[N],
                              logLikelihoodProvider: LogLikelihoodProvider[N],
                              mutationRate: Double,
                              crossoverRate: Double,
                              maxParents: Int) = {
    val rnd = new scala.util.Random(System.currentTimeMillis())

    def pickAtMost(from: Set[N], atMost: Int): Set[N] = {
      if (atMost == 0) Set()
      else {
        val n = rnd.nextInt(atMost)

        if (n >= from.size) from
        else {
          val which = (from.toList)(rnd.nextInt(from.size))
          pickAtMost(from - which, atMost - 1) + which
        }
      }
    }

    val randParentsByNode = (for {
      (node, index) <- allNodes.zipWithIndex
      allowedParents = allNodes.drop(index)
      parents = pickAtMost(allowedParents.toSet, maxParents)
    } yield node -> parents).toSet

    val CPDByNode = randParentsByNode map {
      case (node, parents) => node -> modelProvider.CPDforParents(node, parents)
    } toMap

    BayesNetIndividual(
      BayesNet(CPDByNode),
      modelProvider,
      logLikelihoodProvider,
      mutationRate,
      crossoverRate,
      maxParents
    )
  }
}
