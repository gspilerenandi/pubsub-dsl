package analysis.semantics

import analysis.syntax.{Participant, Program}

object Interpreter {

  def countModes(prog: Program): Int = {
    prog.modes.ms.size
  }
  def countTasksPart(part: Participant): Int = {
    var taskName: Set[String] = Set()
    part.modes.foreach {
      m =>
        m.tasks.foreach {
          t => taskName += t.name
        }
    }
    taskName.size
  }
}
