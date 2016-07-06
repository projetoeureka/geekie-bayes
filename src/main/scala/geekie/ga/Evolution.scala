package geekie.ga


class Evolution[T <: Individual[T]](initialPopulation: Population[T])
                                   (implicit evolutionReport: EvolutionReport = NullEvolutionReport) {

  def run(epochs: Int) = (initialPopulation /: (0 until epochs)) {
    (generation, epoch) =>
      evolutionReport.report(generation.orderedPeople, epoch)
      generation.step()
  }
}

object Evolution {
  def apply[T <: Individual[T]](popSize: Int, eliteSize: Int, selectionSampleSize: Int)
                               (individualFactory: => T)
                               (implicit evolutionReport: EvolutionReport = NullEvolutionReport) = {
    val initIndividuals = (0 until popSize).par.map(_ => individualFactory)
    val initialPopulation = Population(initIndividuals.seq, eliteSize, selectionSampleSize)
    new Evolution(initialPopulation)(evolutionReport)
  }
}
