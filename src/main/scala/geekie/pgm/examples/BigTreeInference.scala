package geekie.pgm.examples

import geekie.pgm.inference.BeliefPropagation
import geekie.pgm.{NodesObservations, CPD, BayesNet}
import geekie.utils.Chronometer


/**
 * In this example you can see how the belief propagation algorithm can handle
 * faily large tree-shaped bayesian network. Since it leverages the fact that
 * nodes have at most one parent, we can run inference in polynomial time.
 *
 * It's worth mentioning that Enumeration and VariableElimination algorithms
 * _cannot_ reasonably handle bayesian networks this size, since they are exponential
 * in the number of nodes.
 */
object BigTreeInference extends App {

  // Let's create a balanced binary tree with height h = 5 (63 nodes)
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
    "A111"  ->  CPD(List("A11"), List(.8, .6)),

    "A0000" ->  CPD(List("A000"), List(.8, .6)),
    "A0001" ->  CPD(List("A000"), List(.8, .6)),
    "A0010" ->  CPD(List("A001"), List(.8, .6)),
    "A0011" ->  CPD(List("A001"), List(.8, .6)),
    "A0100" ->  CPD(List("A010"), List(.8, .6)),
    "A0101" ->  CPD(List("A010"), List(.8, .6)),
    "A0110" ->  CPD(List("A011"), List(.8, .6)),
    "A0111" ->  CPD(List("A011"), List(.8, .6)),

    "A1000" ->  CPD(List("A100"), List(.8, .6)),
    "A1001" ->  CPD(List("A100"), List(.8, .6)),
    "A1010" ->  CPD(List("A101"), List(.8, .6)),
    "A1011" ->  CPD(List("A101"), List(.8, .6)),
    "A1100" ->  CPD(List("A110"), List(.8, .6)),
    "A1101" ->  CPD(List("A110"), List(.8, .6)),
    "A1110" ->  CPD(List("A111"), List(.8, .6)),
    "A1111" ->  CPD(List("A111"), List(.8, .6)),

    "A00000" ->  CPD(List("A0000"), List(.8, .6)),
    "A00001" ->  CPD(List("A0000"), List(.8, .6)),
    "A00010" ->  CPD(List("A0001"), List(.8, .6)),
    "A00011" ->  CPD(List("A0001"), List(.8, .6)),
    "A00100" ->  CPD(List("A0010"), List(.8, .6)),
    "A00101" ->  CPD(List("A0010"), List(.8, .6)),
    "A00110" ->  CPD(List("A0011"), List(.8, .6)),
    "A00111" ->  CPD(List("A0011"), List(.8, .6)),
    "A01000" ->  CPD(List("A0100"), List(.8, .6)),
    "A01001" ->  CPD(List("A0100"), List(.8, .6)),
    "A01010" ->  CPD(List("A0101"), List(.8, .6)),
    "A01011" ->  CPD(List("A0101"), List(.8, .6)),
    "A01100" ->  CPD(List("A0110"), List(.8, .6)),
    "A01101" ->  CPD(List("A0110"), List(.8, .6)),
    "A01110" ->  CPD(List("A0111"), List(.8, .6)),
    "A01111" ->  CPD(List("A0111"), List(.8, .6)),

    "A10000" ->  CPD(List("A1000"), List(.8, .6)),
    "A10001" ->  CPD(List("A1000"), List(.8, .6)),
    "A10010" ->  CPD(List("A1001"), List(.8, .6)),
    "A10011" ->  CPD(List("A1001"), List(.8, .6)),
    "A10100" ->  CPD(List("A1010"), List(.8, .6)),
    "A10101" ->  CPD(List("A1010"), List(.8, .6)),
    "A10110" ->  CPD(List("A1011"), List(.8, .6)),
    "A10111" ->  CPD(List("A1011"), List(.8, .6)),
    "A11000" ->  CPD(List("A1100"), List(.8, .6)),
    "A11001" ->  CPD(List("A1100"), List(.8, .6)),
    "A11010" ->  CPD(List("A1101"), List(.8, .6)),
    "A11011" ->  CPD(List("A1101"), List(.8, .6)),
    "A11100" ->  CPD(List("A1110"), List(.8, .6)),
    "A11101" ->  CPD(List("A1110"), List(.8, .6)),
    "A11110" ->  CPD(List("A1111"), List(.8, .6)),
    "A11111" ->  CPD(List("A1111"), List(.8, .6))
  ))

  val beliefPropagationPredictor = BeliefPropagation(treeNet)

  // Let's infer A110 without evidence
  val obs = NodesObservations() + ("A110", true)

  val (beliefPropagationPrediction, bpTime) =
    Chronometer(beliefPropagationPredictor.infer("A110", NodesObservations[String]()))

  println(s"Belief propagation: P(A110 = true) = ${beliefPropagationPrediction(obs)}, time: ${bpTime}")
}
