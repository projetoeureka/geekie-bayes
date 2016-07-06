package geekie.pgm

import geekie.utils._


case class FreqTable[N](nodes: Seq[N], counts: Seq[Long]) {

  import SeqInsertRemoveMethods._

  val totalCounts = counts.sum

  def getObservationCounts(observation: NodesObservations[N]) =
    counts(BinaryIndexDecoder(nodes map { n => observation(n) }))

  def marginalize(node: N) = {
    val nn = nodes.indexOf(node)
    def newCounts = for {
      aa <- 0 until (1 << (nodes.size - 1))
      qq = BinaryIndexEncoder(aa, nodes.size - 1)
      ia = BinaryIndexDecoder(qq.insert(nn, false))
      ib = BinaryIndexDecoder(qq.insert(nn, true))
    } yield counts(ia) + counts(ib)
    FreqTable(nodes.remove(nn), newCounts)
  }

  def getCPD(targetNode: N): CPD[N] = {
    val parents = nodes.filter(_ != targetNode).toList

    val probabilities = NodesObservations.allPossibleObservations(parents) map {
      obs =>
        val cTrue = getCounts(obs + (targetNode, true))
        val cFalse = getCounts(obs + (targetNode, false))
        cTrue.toDouble / (cTrue + cFalse)
    }

    CPD(parents, probabilities.toList)
  }

  def getCounts(obs: NodesObservations[N]): Long = {
    if (obs.keySet != nodes.toSet) throw new IllegalArgumentException("All observations must be supplied")

    val index = obs.foldLeft(0) {
      case (acc, (node, value)) =>
        acc + (if (value) 1 << (nodes.length - 1 - nodes.indexOf(node)) else 0)
    }
    counts(index)
  }

  def +(that: FreqTable[N]) = {
    assert(this.nodes == that.nodes, "The nodes are different or in a different order")
    FreqTable[N](this.nodes, (this.counts, that.counts).zipped map (_ + _))
  }
}


object FreqTable {
  def fromBoolean[N](nodes: Seq[N], choices: Seq[Boolean]): FreqTable[N] = {
    val counts = List.fill(1 << nodes.length)(0L).updated(BinaryIndexDecoder(choices), 1L)
    FreqTable[N](nodes, counts)
  }

  /**
   * This is a shortcut for generating a ModelProvider from a sequence of
   * NodesObservations. This is useful for tests only. For real-world applications,
   * you're gonna want a better way of generating FreqTables from observations, since
   * this process is exponential in the number of nodes in sample.
   *
   * @param sample
   * @param maxParents
   * @tparam N
   * @return
   */
  def combinationsFromSample[N](sample: List[NodesObservations[N]], maxParents: Int): Iterator[FreqTable[N]] = {

    val allNodes = sample.head.keys.toList

    def makeFreqTable(nodes: List[N]) = {
      val defaultFreqTable = FreqTable(nodes, List.fill(1 << nodes.length)(0L))

      (defaultFreqTable /: sample) {
        case (FreqTable(nn, count), observation) =>
          val index = BinaryIndexDecoder(nodes.map(observation(_)))
          FreqTable(nn, count.updated(index, count(index) + 1))
      }
    }

    // All possible combinations of freqTables with up to maxParents parents
    val freqTablesIterator: Iterator[FreqTable[N]] = for {
      nNodes <- (1 to (maxParents + 1)).toIterator
      combination <- allNodes.combinations(nNodes)
    } yield makeFreqTable(combination)

    freqTablesIterator
  }
}

