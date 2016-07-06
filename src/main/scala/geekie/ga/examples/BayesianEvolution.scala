package geekie.ga.examples

import geekie.ga.{BasicEvolutionReport, BayesNetIndividual, Evolution}
import geekie.pgm._
import geekie.utils.{BasicProgressReport, Chronometer}


object BayesianEvolution extends App {
  //1 - generate a known model and sample
  val net = BayesNet[String](Map(
    "R" -> CPD(List[String](), List(0.5)),
    "E" -> CPD(List[String](), List(0.5)),
    "A" -> CPD(List("R", "E"), List(0.001, 0.29, 0.94, 0.95)),
    "J" -> CPD(List("A"), List(0.05, 0.9)),
    "M" -> CPD(List("A"), List(0.01, 0.7))
  ))

  val epochs = 20
  val popSize = 300
  val eliteSize = 2
  val selectionSampleSize = 10
  val mutationRate = .5
  val crossoverRate = .9
  val maxParents = 1

  implicit val evolutionReport = BasicEvolutionReport

  val trainingSampleSize = 10000
  val testSampleSize = 500

  val trainingFreqTables = FreqTable.combinationsFromSample(
    List.fill(trainingSampleSize)(net.drawObservation), maxParents)

  val testFreqTables = FreqTable.combinationsFromSample(
    List.fill(testSampleSize)(net.drawObservation), maxParents)

  val modelProvider = ModelProvider(trainingFreqTables)
  val logLikelihoodProvider = LogLikelihoodProvider(modelProvider, testFreqTables.toList)

  println("Creating initial population...")
  val evolution = Evolution(popSize, eliteSize, selectionSampleSize) {
    BayesNetIndividual.makeRandomIndividualOld(
      modelProvider.scope.toList,
      modelProvider,
      logLikelihoodProvider,
      mutationRate,
      crossoverRate,
      maxParents)
  }

  println("Running evolution...")
  evolution.run(epochs)
}
