object Calculator {

  abstract class Expression {
    def calculate: Int = this match {
      case Number(n) => n
      case Multiply(l, r) => l.calculate * r.calculate
      case Divide(l, r) => l.calculate / r.calculate
      case Subtract(l, r) =>  l.calculate - r.calculate
      case Add(l, r) => l.calculate + r.calculate
    }
  }

  case class Number(n: Int) extends Expression
  case class Add(l: Expression, r: Expression) extends Expression
  case class Subtract(l: Expression, r: Expression) extends Expression
  case class Multiply(l: Expression, r: Expression) extends Expression
  case class Divide(l: Expression, r: Expression) extends Expression

}

object Test {
  def run: Unit = {
    val c = Calculator
    val exp = "1 + 2 * 3"
    c.Number(2)
    println(c.Add(c.Number(2), c.Number(2)).calculate == 4)
    println(c.Add(c.Multiply(c.Number(2), c.Number(2)), c.Number(2)).calculate == 6)
    println(c.Subtract(c.Multiply(c.Number(2), c.Number(2)), c.Number(2)).calculate == 2)
    println(c.Divide(c.Multiply(c.Number(2), c.Number(2)), c.Number(2)).calculate == 2)
  }
}


object Main extends App {
  Test.run
}
