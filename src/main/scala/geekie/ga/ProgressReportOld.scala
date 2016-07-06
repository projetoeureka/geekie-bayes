package geekie.ga


case class ProgressReportOld(best: Double, worst: Double, median: Double, mean: Double) {
  def toList = List[Double](best, worst, median, mean)
}

