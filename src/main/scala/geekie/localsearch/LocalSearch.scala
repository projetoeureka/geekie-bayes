package geekie.localsearch

import geekie.utils.{ProgressReport, NullProgressReport}

import scala.annotation.tailrec

/**
 * Simple hill-climbing algorithm to find a local optimum.
 *
 * @tparam T
 */
class LocalSearch[T <: LocallyOptimizable[T]](implicit val progressReporter: ProgressReport = NullProgressReport) {

  final def apply(locallyOptimizable: T): T = {
    recursiveSearch(locallyOptimizable, 0)
  }

  @tailrec
  private def recursiveSearch(locallyOptimizable: T, iteration: Int): T = {
    progressReporter.report(s"Hill-climbing again on ${locallyOptimizable}", iteration, -1)

    val bestNeighbors = locallyOptimizable.neighborhood.maxBy(_.objective)

    if (locallyOptimizable.objective >= bestNeighbors.objective) {
      progressReporter.report(s"Found local optimium on ${locallyOptimizable}", iteration, -1)
      locallyOptimizable
    }
    else {
      recursiveSearch(bestNeighbors, iteration + 1)
    }
  }
}
