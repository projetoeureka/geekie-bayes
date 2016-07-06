package geekie.pgm.examples

import geekie.pgm.inference.{BeliefPropagation, Enumeration, VariableElimination}
import geekie.pgm.{NodesObservations, CPD, BayesNet}
import geekie.utils.Chronometer


/**
 * In this example you can see the comparison of the tree inference algorithms
 * on a fairly small tree-shaped bayesian network. This goes to show that the
 * belief propagation algorithm produces the same answer as the other two, but
 * since it is polynomial in the number of nodes, we can handle much bigger trees.
 * See examples.BigTreeInference.
 *
 */
object TreeNetInferenceBenchmark extends App {

  // Let's create a balanced binary tree with height h = 3 (15 nodes)
  val treeNet = BayesNet[String](Map(
    "A"     ->  CPD(List(), List(.9)),
    "A0"    ->  CPD(List("A"), List(.8, .6)),
    "A1"    ->  CPD(List("A"), List(.8, .6)),
    "A00"   ->  CPD(List("A0"), List(.8, .6)),
    "A01"   ->  CPD(List("A0"), List(.8, .6)),
    "A10"   ->  CPD(List("A1"), List(.8, .6)),
    "A11"   ->  CPD(List("A1"), List(.8, .6)),
    "A000"  ->  CPD(List("A00"), List(.8, .6)),
    "A001"  ->  CPD(List("A00"), List(.8, .6)),
    "A010"  ->  CPD(List("A01"), List(.8, .6)),
    "A011"  ->  CPD(List("A01"), List(.8, .6)),
    "A100"  ->  CPD(List("A10"), List(.8, .6)),
    "A101"  ->  CPD(List("A10"), List(.8, .6)),
    "A110"  ->  CPD(List("A11"), List(.8, .6)),
    "A111"  ->  CPD(List("A11"), List(.8, .6))
  ))

  val enumerationPredictor = Enumeration(treeNet)
  val variableEliminationPredictor = VariableElimination(treeNet)
  val beliefPropagationPredictor = BeliefPropagation(treeNet)

  // Let's infer A110 without evidence
  val obs = NodesObservations() + ("A110", true)

  val (enumerationPrediction, enTime) =
    Chronometer(enumerationPredictor.infer("A110", NodesObservations[String]()))

  println(s"Enumeration: P(A110 = true) = ${enumerationPrediction(obs)}, time: ${enTime}")

  val (variableEliminationPrediction, veTime) =
    Chronometer(variableEliminationPredictor.infer("A110", NodesObservations[String]()))

  println(s"Variable elimination: P(A110 = true) = ${variableEliminationPrediction(obs)}, time: ${veTime}")

  val (beliefPropagationPrediction, bpTime) =
    Chronometer(beliefPropagationPredictor.infer("A110", NodesObservations[String]()))

  println(s"Belief propagation: P(A110 = true) = ${beliefPropagationPrediction(obs)}, time: ${bpTime}")
}
