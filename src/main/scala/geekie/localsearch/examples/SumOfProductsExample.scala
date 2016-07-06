package geekie.localsearch.examples

import geekie.localsearch.{LocallyOptimizable, LocalSearch}

import scala.language.postfixOps
import scala.util.Random

/**
 * For a given array of Int, find the ordenation that maximizes the sum of the product of neighbors
 * e.g: [1,3,4,2] = (1*3) + (3*4) + (4*2) = 23
 * Using local search to find the best ordenation
 */


object SumOfProductsExample extends App {

  case class SumOfProducts(lst: Seq[Int]) extends LocallyOptimizable[SumOfProducts] {
    def objective = (lst.tail zip lst).map({ case (a, b) => a * b }).sum.toDouble

    def neighborhood = {
      (for {
        el <- lst
        index <- lst.indices if index != lst.indexOf(el)
        xMinusEl = lst diff List(el)
      } yield SumOfProducts((xMinusEl.take(index) :+ el) ++ xMinusEl.slice(index, lst.length))).toList
    }
    
    override def toString = s"Objective: $objective => $lst"
  }

  val lSearch = new LocalSearch[SumOfProducts]

  val initialState = SumOfProducts(Random.shuffle((1 to 10).toList))

  val sol = lSearch(initialState)
  println(s"Found solution  : $sol")

  val opt = SumOfProducts(Seq(2, 4, 6, 8, 10, 9, 7, 5, 3, 1))
  println(s"Optimal solution: $opt")
}
