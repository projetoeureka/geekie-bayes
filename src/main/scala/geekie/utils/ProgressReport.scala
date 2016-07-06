package geekie.utils


trait ProgressReport {
  def report(taskName: String, doneTasks: Int, totalTasks: Int): Unit
}


object NullProgressReport extends ProgressReport {
    override def report(taskName: String, doneTasks: Int, totalTasks: Int): Unit = {}
}


object BasicProgressReport extends ProgressReport {
  override def report(taskName: String, doneTasks: Int, totalTasks: Int): Unit =
    println(s"${taskName}: $doneTasks/$totalTasks")
}
