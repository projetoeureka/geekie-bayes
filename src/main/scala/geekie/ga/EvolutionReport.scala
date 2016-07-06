package geekie.ga


trait EvolutionReport {
  def report(currentIndivs: Seq[Individual[_]], currentEpoch: Int): Unit
}

object NullEvolutionReport extends EvolutionReport {
  override def report(currentIndivs: Seq[Individual[_]], currentEpoch: Int): Unit = {}
}

object BasicEvolutionReport extends EvolutionReport {
  def report(orderedPeople: Seq[Individual[_]], currentEpoch: Int): Unit = {
    val best = orderedPeople.head.fitness
    val worst = orderedPeople.last.fitness
    val mean = orderedPeople.map(_.fitness).sum / orderedPeople.length.toDouble
    val median = orderedPeople(orderedPeople.length / 2).fitness

    println(s"Epoch $currentEpoch. Top 5 individuals:")
    orderedPeople.take(5).foreach(println)
    println(s"Best:   $best")
    println(s"Worst:  $worst")
    println(s"Mean:   $mean")
    println(s"Median: $median")
  }
}
