package geekie.pgm.inference

import geekie.pgm.{NodesObservations, BayesNet}

import scala.annotation.tailrec
import scala.collection.immutable.Queue


sealed trait Message[N] {
  val from: N
  val to: N
  val content: Factor[N]
  val evidenceNodes: Set[N]
}

case class rMessage[N](from: N, to: N, content: Factor[N], evidenceNodes: Set[N]) extends Message[N]
case class PMessage[N](from: N, to: N, content: Factor[N], evidenceNodes: Set[N]) extends Message[N]


/**
 * The belief propagation algorithm is a message passing inference algorithm. This implementation
 * comes directly from pearl82.
 *
 * @param bayesNet
 * @tparam N
 */
case class BeliefPropagation[N <% Ordered[N]](private val bayesNet: BayesNet[N]) extends InferenceAlgorithm[N] {

  private def initialProcessorsByNode: Map[N, Processor[N]] = {
    val processorByNode = bayesNet.CPDbyNode map {
      case (node, cpd) =>
        val nodeParents = bayesNet.dag.parents(node)
        val nodeChildren = bayesNet.dag.children(node)

        if (nodeParents.size > 1) {
          throw new IllegalArgumentException(s"Node ${node} has more than one parent!")
        }
        else (node, Processor(node, cpd, nodeParents, nodeChildren))
    }

    // Let's try to reach the equilibrium without any evidence first.

    // 1. We don't need to run through all the leaves, since we are already
    // initializing all `lambdas` to (1, 1).

    // 2. Run through all roots and propagate their prior
    val initialMessages = for {
      (node, processor) <- processorByNode
      if processor.parents.isEmpty
      fac = Factor(node, List(1d, 1d))
      msg = PMessage(node, node, fac, Set())
    } yield processor.handleAndGenerateMessages(msg)

    processMessageQueue(Queue.empty ++ initialMessages.flatten, processorByNode)
  }

  /**
   * When `infer` is called, processors for each node in the tree will be created and
   * initial messages will be propagated until the tree reaches equilibrium. Then, for each
   * evidence, the procedure will propagate the effect of that evidence to the entire
   * network before dealing with the next evidence. At the end of the process,
   * every node's `Processor` holds the propagated belief of all evidence. That is,
   * each node's marginal propability distribution is available on its processor.
   *
   * @param query
   * @param evidence
   * @return
   */
  override def infer(query: Set[N], evidence: NodesObservations[N]): Factor[N] = {
    if (query.size != 1) throw new UnsupportedOperationException("Belief propagation inference is only defined one query variable!")

    val msgs = for {
      (node, value) <- evidence
       content = Factor(node, if (value) List(0d, 1d) else List(1d, 0d))
    } yield rMessage(node, node, content, evidence.keySet)

    val propagatedProcessorByNode = processMessageQueue(Queue.empty ++ msgs, initialProcessorsByNode)

    // Return the query's marginal distribution
    propagatedProcessorByNode(query.head).P
  }

  @tailrec
  private def processMessageQueue(msgs: Queue[Message[N]], processorByNode: Map[N, Processor[N]]): Map[N, Processor[N]] = msgs match {
    case msg +: rest =>
      processMessageQueue(rest.enqueue(processorByNode(msg.to).handleAndGenerateMessages(msg)), processorByNode)

    case Queue() => processorByNode
  }
}
