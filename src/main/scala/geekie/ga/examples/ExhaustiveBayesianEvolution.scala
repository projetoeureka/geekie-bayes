package geekie.ga.examples

import geekie.ga.BayesNetIndividual
import geekie.pgm._

/**
 *  In this example, we exhaustively generate all possible nets and calculate
 *  their fitness. We hope that similar nets to the one used to generate the
 *  training and testing data will rank higher on the final list.
 */
object ExhaustiveBayesianEvolution extends App {
  val net = BayesNet[String](Map(
    "R" -> CPD(List[String](), List(0.5)),
    "E" -> CPD(List[String](), List(0.5)),
    "A" -> CPD(List("R", "E"), List(0.001, 0.29, 0.94, 0.95)),
    "J" -> CPD(List("A"), List(0.05, 0.9)),
    "M" -> CPD(List("A"), List(0.01, 0.7))
  ))

  val maxParents = 1
  val mutationRate = .1
  val crossoverRate = .5
  val trainingSampleSize = 10000
  val testSampleSize = 1000

  val trainingFreqTables = FreqTable.combinationsFromSample(
    List.fill(trainingSampleSize)(net.drawObservation), maxParents)

  val testFreqTables = FreqTable.combinationsFromSample(
    List.fill(testSampleSize)(net.drawObservation), maxParents)

  val modelProvider = ModelProvider(trainingFreqTables)
  val logLikelihoodProvider = LogLikelihoodProvider(modelProvider, testFreqTables.toList)

  // Make all possible networks
  val allIndiv = AllNets.allNets[String](modelProvider.scope.toList, maxParents) map {
    case (dag) =>
      val cpdByNode = (dag.nodes map { node => node -> modelProvider.CPDforParents(node, dag.parents(node)) }).toMap
      BayesNetIndividual(
        BayesNet(cpdByNode),
        modelProvider,
        logLikelihoodProvider,
        mutationRate,
        crossoverRate,
        maxParents)
  }

  allIndiv sortBy (_.fitness) foreach println
}
