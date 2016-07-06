package geekie.ga.examples

import geekie.ga.Individual

case class DumbNet(dna: List[Int]) extends Individual[DumbNet] {
  val fitness: Double = dna.sum
  val mutationAttempts = 20
  val mutationRate = 0.05
  val genomeDiversity = 1980
  val rnd = new scala.util.Random(System.currentTimeMillis)

  def crossover(other: DumbNet) = {
    val bagOfGenes = (dna ++ other.dna).distinct
    DumbNet(rnd.shuffle(bagOfGenes).take(dna.length))
  }

  def mutate = {
    def mutateGene(dna: List[Int]) = {
      val viableGenes = (0 to genomeDiversity).filterNot(dna.contains(_))
      val newGene = viableGenes(rnd.nextInt(viableGenes.size))
      val pos = rnd.nextInt(dna.size)
      dna.updated(pos, newGene)
    }

    val mutatedDna = (dna /: (1 to mutationAttempts)) {
      (d, _) => if (rnd.nextDouble < mutationRate) mutateGene(d) else d
    }

    DumbNet(mutatedDna)
  }

  override def toString = f"$fitness\t${dna.sorted}"
}
