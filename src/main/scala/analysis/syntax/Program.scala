package analysis.syntax

case class Program(participants:List[Participant], modes:Modes)

case class Participant(name:String, sched:Scheduler, modes:Set[ModeBlock])

case class Topic(name:String, publishers: Set[Participant], subscribers: Set[Participant])

case class Task(name: String, schedParams: SchedParams)

sealed abstract class Mode
case object AnyOtherMode extends Mode
case class SomeMode(name: String) extends Mode
// probably not necessary
case class Modes(ms:Set[String])

case class ModeBlock(mode:Mode, tasks:Set[Task])

sealed abstract class Decl
case class ExternalDecl(name:String, ins:List[Type], out:Type) extends Decl
case class FieldDecl(name:String, typ:Type, value:String)

// missing an optional priority parameter
case class SchedParams(p:ValueAndUnit, d:ValueAndUnit, wcet:ValueAndUnit)

case class ValueAndUnit(value:String, timeUnit: String)

case class Type(name:String)

case class Scheduler(name:String)

sealed abstract class Statement
case class Assignment(variable:String,
                      expr:Expr) extends Statement

sealed abstract class Expr extends Statement
case class MethodCall(quantifier:List[String],
                      name:String,
                      args:List[Expr]) extends Expr
case class ITE(ifc:Expr, thenc:Expr, elsec:Option[Expr]) extends Expr
sealed class Value extends Expr
case class IntV(int:Int) extends Value
case class BoolV(bool:Int) extends Value
case class Variable(name:String) extends Expr

case class InfixOp(op:String,left:Expr,right:Expr)