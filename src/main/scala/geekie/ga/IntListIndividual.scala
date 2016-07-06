package geekie.ga

/**
 * Represents a simple individual whose DNA is simply a list of distinct integers.
 * The fitness is simply the sum of all integers on that list.
 * This class is useful for debug and testing of the evolution mechanism
 *
 * @param dna
 */
case class IntListIndividual(dna: List[Int],
                             mutationRate: Double,
                             genomeDiversity: Int) extends Individual[IntListIndividual] {
  val fitness: Double = dna.sum

  private val mutationAttempts = 20
  private val rnd = new scala.util.Random(System.currentTimeMillis)

  def crossover(other: IntListIndividual) = {
    val bagOfGenes = (dna ++ other.dna).distinct
    IntListIndividual(rnd.shuffle(bagOfGenes).take(dna.length), mutationRate, genomeDiversity)
  }

  def mutate = {
    def mutateGene(dna: List[Int]) = {

      // Favors genes that are not in this dna
      val newGene = rnd.nextInt(genomeDiversity)
      val pos = rnd.nextInt(dna.size)
      dna.updated(pos, newGene)
    }

    val mutatedDna = (dna /: (1 to mutationAttempts)) {
      (d, _) => if (rnd.nextDouble < mutationRate) mutateGene(d) else d
    }

    IntListIndividual(mutatedDna, mutationRate, genomeDiversity)
  }

  override def toString = f"$fitness\t${dna.sorted}"
}
