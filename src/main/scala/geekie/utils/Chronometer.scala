package geekie.utils

import scala.concurrent.duration._

object Chronometer {
  def apply[R](block: => R) = {
    val startTime = System.nanoTime()
    val result: R = block
    val endTime = System.nanoTime()
    (result, Duration(endTime - startTime, NANOSECONDS))
  }
}
