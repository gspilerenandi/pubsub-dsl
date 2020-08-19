package analysis.syntax
import common.ParsingException

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {

  def parseProgram(code: String): Program = {
    parseAll(program, code) match {
      case Success(result, _) => result
      case f: NoSuccess => throw new ParsingException("Parser failed: " + f)
    }
  }

  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  // ids that begin with an upper case
  val upperCaseId: Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  // ids that begin with a lower case
  val lowerCaseId: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  // ids that start with both lower and upper case
  val id: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  // text that only contain lowercase letters
  val timeUnit: Parser[String] = """[a-z]*""".r
  // unsigned integers
  val intValue: Parser[String] = """[0-9]*""".r

  val keywords: Set[String] =
    Set("participant", "topic", "scheduler", "mode", "rm", "task", "period", "deadline", "wcet", "priority")


  def program: Parser[Program] =
    modes ~ rep(participant) ^^ {
      case m ~ p => Program(p, m)
    }

  def participant: Parser[Participant] =
    "participant" ~ id ~ "{" ~ scheduler ~ rep(modeBlock) ~ "}" ^^ {
      case _ ~ name ~ _ ~ sched ~ modes ~ _ => Participant(name, sched, modes.toSet)
    }

  def scheduler: Parser[Scheduler] =
    "scheduler" ~ lowerCaseId ~ ";" ^^ {
      case _ ~ name ~ _ => Scheduler(name)
    }

  def task: Parser[Task] =
    "task" ~ lowerCaseId ~ "{" ~ schedParams ~ "};" ^^ {
      case _ ~ name ~ _ ~ sp ~ _ => Task(name, sp)
    }

  def modes: Parser[Modes] =
    "Modes" ~ "{" ~ repsep(upperCaseId, ",") ~ "};" ^^ {
      case _ ~ _ ~ m ~ _ => Modes(m.toSet)
    } |
      "Modes" ~ "{" ~ upperCaseId ~ "};" ^^ {
        case _ ~ _ ~ m ~ _ => Modes(Set(m))
      }

  def modeBlock: Parser[ModeBlock] =
    "mode" ~ upperCaseId ~ "{" ~ rep(task) ~ "}" ^^ {
      case _ ~ e ~ _ ~ tasks ~ _ => ModeBlock(SomeMode(e), tasks.toSet)
    } |
      "mode" ~ "_" ~ "{" ~ rep(task) ~ "}" ^^ {
        case _ ~ _ ~ _ ~ tasks ~ _ => ModeBlock(AnyOtherMode, tasks.toSet)
      } //|
  //block ^^ (tasks => ModeBlock(AnyOtherMode,tasks))

  def valUnit: Parser[ValueAndUnit] =
    intValue ~ timeUnit ^^ {
      case v ~ u => ValueAndUnit(v, u)
    }

  // missing option priority
  def schedParams: Parser[SchedParams] =
    "period" ~ "=" ~ valUnit ~ "," ~ "deadline" ~ "=" ~ valUnit ~ "," ~ "wcet" ~ "=" ~ valUnit ^^ {
      case _ ~ _ ~ period ~ _ ~ _ ~ _ ~ deadline ~ _ ~ _ ~ _ ~ wcet => SchedParams(period, deadline, wcet)
    }
}
