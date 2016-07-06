package geekie.pgm.examples

import geekie.pgm.inference.{NaiveBayes, VariableElimination, Enumeration}
import geekie.pgm.{CPD, BayesNet}


object NaiveBayesNetInference extends App {

  val naiveNet = BayesNet[String](Map(
    "A"  ->  CPD(List(), List(.5)),
    "B1" ->  CPD(List("A"), List(.4, .6)),
    "B2" ->  CPD(List("A"), List(.4, .6)),
    "B3" ->  CPD(List("A"), List(.4, .6)),
    "B4" ->  CPD(List("A"), List(.4, .6)),
    "B5" ->  CPD(List("A"), List(.4, .6)),
    "B6" ->  CPD(List("A"), List(.4, .6)),
    "B7" ->  CPD(List("A"), List(.4, .6)),
    "B8" ->  CPD(List("A"), List(.4, .6)),
    "B9" ->  CPD(List("A"), List(.4, .6)),
    "B10" -> CPD(List("A"), List(.4, .6)),
    "B11" -> CPD(List("A"), List(.4, .6)),
    "B12" -> CPD(List("A"), List(.4, .6)),
    "B13" -> CPD(List("A"), List(.4, .6)),
    "B14" -> CPD(List("A"), List(.4, .6)),
    "B15" -> CPD(List("A"), List(.4, .6)),
    "B16" -> CPD(List("A"), List(.4, .6)),
    "B17" -> CPD(List("A"), List(.4, .6)),
    "B18" -> CPD(List("A"), List(.4, .6)),
    "B19" -> CPD(List("A"), List(.4, .6)),
    "B20" -> CPD(List("A"), List(.4, .6)),
    "B21" -> CPD(List("A"), List(.4, .6))
  ))

  // Inference algorithms
  val naiveBayesPredictor          = NaiveBayes(naiveNet)
  val enumerationPredictor         = Enumeration(naiveNet)
  val variableEliminationPredictor = VariableElimination(naiveNet)

  // Let's draw an observation of this network. Behind the scenes, `naiveNet.drawObservation()` will sort
  // the nodes topologically and draw conditional observations for the dependent nodes.
  val originalNodesObservations = naiveNet.drawObservation()

  // Now, let's pretend we haven't observed the value of node `A`
  val testNodesObservations = originalNodesObservations - "A"

  // How well can we predict the value of `A` using all other observations?
  // Let's try out the three following inference algorithms. (Hint: they must yield the same results!)
  val naivePrediction       = naiveBayesPredictor.infer("A", testNodesObservations)
  val enumerationPrediction = enumerationPredictor.infer("A", testNodesObservations)
  val variableEliminationPrediction = variableEliminationPredictor.infer("A", testNodesObservations)

  // Predictors return instance of `Factor`, which contain, repectively,
  // (probability of `A` being false, probability of `A` being true)
  println(
    s"""
       |Real value for `A`: ${originalNodesObservations("A")}
       |Naive prediction (P_false, P_true): ${naivePrediction}
       |Enumeration prediction (P_false, P_true): ${enumerationPrediction}
       |Variable elimination prediction (P_false, P_true): ${variableEliminationPrediction}
     """.stripMargin)
}
