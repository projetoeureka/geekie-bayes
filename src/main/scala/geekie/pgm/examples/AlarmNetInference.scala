package geekie.pgm.examples

import geekie.pgm.inference.{VariableElimination, Enumeration}
import geekie.pgm.{NodesObservations, CPD, BayesNet}

object AlarmNetInference extends App {

  /*
   Consider the famous "Alarm network", where the nodes represent:
    B - Burglary happens
    E - Earthquake happens
    A - Alarm goes off
    J - John calls us to warn  that the alarm went off
    M - Mary calls us to warn that the alarm went off
  */
  val alarmNet = BayesNet[String](Map(
    "B" -> CPD(List(), List(.001)),
    "E" -> CPD(List(), List(.002)),
    "A" -> CPD(List("B", "E"), List(.001, .29, .94, .95)),
    "J" -> CPD(List("A"), List(.05, .9)),
    "M" -> CPD(List("A"), List(.01, .7))
  ))

  // Say we are interested in finding the probability that a Burglary happened given that Mary AND John called us.
  // Let's use two inference algorithms. They should yield the same results.
  val enumerationPredictor = Enumeration(alarmNet)
  val variableEliminationPredictor = VariableElimination(alarmNet)

  // The evidence is, as stated, that Mary and John called, that is, M -> true, J -> true
  val evidence = NodesObservations() + ("M", true) + ("J", true)

  val burglaryEnumPrediction = enumerationPredictor.infer("B", evidence)
  val burglaryVEPrediction = variableEliminationPredictor.infer("B", evidence)

  println(
    s"""
      |The probability that a Burglary happened is:
      |By enumeration algorithm (P_false, P_true): ${burglaryEnumPrediction}
      |By variable elimination algorithm (P_false, P_true): ${burglaryVEPrediction}
    """.stripMargin)

}
