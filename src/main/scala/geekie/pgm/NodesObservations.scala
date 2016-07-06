package geekie.pgm

import geekie.utils.BooleanCombinations

case class NodesObservations[N](value: Map[N, Boolean]) extends AnyRef with Iterable[(N, Boolean)] {
  def apply(key: N) = value.apply(key)

  def keys = value.keys

  def keySet = value.keySet

  // TODO: convert these signatures to
  //def filterKeys(p: (N) => Boolean): NodesObservations
  val filterKeys = value.filterKeys _

  def iterator = value.iterator

  val filter = value.filter _

  def -(key: N) = NodesObservations(value - key)

  def +(elem: (N, Boolean)) = NodesObservations(value + elem)

  def ++(newObs: NodesObservations[N]): NodesObservations[N] = NodesObservations(value ++ newObs.value)
}

object NodesObservations {
  def apply[N](): NodesObservations[N] = NodesObservations[N](Map[N, Boolean]())

  def allPossibleObservations[N](nodes: List[N]): Seq[NodesObservations[N]] =
    BooleanCombinations(nodes.length) map { comb => NodesObservations[N]((nodes zip comb).toMap) }
}
