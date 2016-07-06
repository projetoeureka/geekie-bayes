package geekie.pgm.inference

import geekie.pgm.CPD

import scala.collection.mutable


/**
 * Processor is the "processing unit" in the belief propagation algorithm.
 * Each node has a processor associated with it, and the processor is responsible
 * for managing the node's internal beliefs as well as computing messages to its
 * parents and its children.
 *
 * @param node
 * @tparam N
 */
case class Processor[N <% Ordered[N]](node: N, cpd: CPD[N], parents: Set[N], children: Set[N]) {

  // `M` is `node`'s CPD itself
  private val M: Factor[N] = Factor(node, cpd)

  // `lambda` is the "evidential support" provided to this node from its descendants
  var lambda: Factor[N] = Factor(node, List(1d, 1d))

  // `q` is the "anticipatory support" provided to this node from its ancestors
  var q: Factor[N] =
    if (parents.nonEmpty) Factor(node, List(1d, 1d))
    else M

  // Map holding current messages received by each child
  val rByChild = mutable.Map() ++ (children map { case(child) => child -> Factor(node, List(1d, 1d)) } toMap)

  // `P`: message to be passed to children
  def P: Factor[N] = (q * lambda).normalize

  // `r`: message to be passed to parent
  def r: Factor[N] = (M * lambda).sumout(node)

  // Update this `Processor`'s internal state (`lambda` and `q`) and generate
  // messages for it's children and parent
  def handleAndGenerateMessages(msg: Message[N]): Set[Message[N]] = {
    msg match {
      case msg: rMessage[N] =>

        // Evidences will received a Dirac's delta `r` from itself,
        // which will always discard other `r`s when computing `lambda`
        rByChild(msg.from) = msg.content
        lambda = rByChild.values.reduce(_ * _)

      case msg: PMessage[N] =>

        // The only time root will received a `P` message will be upon tree
        // initialization, when it will propagate its prior to the rest of the tree
        if (parents.nonEmpty)
          q = (msg.content * M).sumout(msg.from)
    }

    val msgsToChildren = children filter(_ != msg.from) map {
      case child => PMessage(node, child, P, msg.evidenceNodes)
    }
    val msgToParent = parents filter(_ != msg.from) map {
      case parent => rMessage(node, parent, r, msg.evidenceNodes)
    }

    msgsToChildren ++ msgToParent
  }
}

