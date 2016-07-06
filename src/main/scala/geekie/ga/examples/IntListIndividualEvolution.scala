package geekie.ga.examples

import geekie.ga.{BasicEvolutionReport, Evolution, Population, IntListIndividual}


object IntListIndividualEvolution extends App {

  val rnd = new scala.util.Random(System.currentTimeMillis)
  val epochs = 2000
  val popSize = 500
  val eliteSize = 50
  val selectionSampleSize = 10

  val genomeSize = 100
  val geneRange = 2048
  val mutationRate = .05

  implicit val evolutionReport = BasicEvolutionReport

  val initialPopulation = makeIntListPopulation

  def makeIntListPopulation = {
    def randomIndividual = {
      val genome = List.fill(genomeSize)(rnd.nextInt(geneRange))
      IntListIndividual(genome, mutationRate, geneRange)
    }
    List.fill(popSize)(randomIndividual)
  }

  val population = new Population[IntListIndividual](initialPopulation, eliteSize, selectionSampleSize)

  new Evolution(population).run(epochs)
}
