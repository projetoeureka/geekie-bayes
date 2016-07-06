package geekie.localsearch

trait LocallyOptimizable[T <: LocallyOptimizable[T]] {

  def neighborhood: List[T]

  def objective: Double
}
