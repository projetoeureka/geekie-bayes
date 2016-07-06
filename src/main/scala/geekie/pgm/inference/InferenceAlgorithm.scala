package geekie.pgm.inference

import geekie.pgm.{NodesObservations, CPD}


trait InferenceAlgorithm[N] {
  def infer(query: Set[N], evidence: NodesObservations[N]): Factor[N]
  def infer(query: N, evidence: NodesObservations[N]): Factor[N] = infer(Set(query), evidence)
}
