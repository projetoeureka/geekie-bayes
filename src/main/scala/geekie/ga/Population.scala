package geekie.ga


case class Population[T <: Individual[T]](people: Seq[T],
                                          eliteSize: Int,
                                          selectionSampleSize: Int) {

  val popSize = people.length
  assert(selectionSampleSize <= popSize)
  assert(eliteSize <= popSize)

  val rnd = new scala.util.Random(System.currentTimeMillis)

  // Individuals with best fitness should come first
  lazy val orderedPeople = people.sortBy(-_.fitness)

  orderedPeople.head.fitness

  def step(): Population[T] = {
    val elite = orderedPeople.take(eliteSize)

    def naturalSelection(size: Int) = {
      def pickOne(): T = rnd.shuffle(people).take(selectionSampleSize).sortBy(-_.fitness).head

      def choosePairs = for {i <- (0 to size).par} yield {
        pickOne().reproduce(pickOne())
      }
      choosePairs.toList
    }

    val newPop = elite ++ naturalSelection(popSize - eliteSize)
    new Population(newPop, eliteSize, selectionSampleSize)
  }

  def getBest(amount: Int) = {
    orderedPeople.take(amount)
  }
}
