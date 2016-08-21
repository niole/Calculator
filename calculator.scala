object Calculator {
  //type Unparsed = List[String]
  type Unparsed = List[Token]
  type Parsed = (Unparsed, Expression)

  class Expression {

    def number(n: String): Expression = Number(n.toInt)

    def calculate: Int = this match {
      case Number(n) => n
      case Multiply(l, r) => l.calculate * r.calculate
      case Divide(l, r) => l.calculate / r.calculate
      case Subtract(l, r) =>  l.calculate - r.calculate
      case Add(l, r) => l.calculate + r.calculate
    }

    def print: String = this match {
      case Number(n) => n.toString
      case Multiply(l, r) => l.print + " * " + r.print
      case Divide(l, r) => l.print + " / " + r.print
      case Subtract(l, r) =>  l.print + " - " + r.print
      case Add(l, r) => l.print + " + " + r.print
    }
  }

  def getAgg(op: String): (Expression, Expression) => Expression = op match {
    case "+" => (x: Expression, y: Expression) => Add(x,y)
    case "-" => (x: Expression, y: Expression) => Subtract(x,y)
    case "*" => (x: Expression, y: Expression) => Multiply(x,y)
    case "/" => (x: Expression, y: Expression) => Divide(x,y)
  }

  def run(input: String): Expression = {
    val tokens = tokenize(input.split(" ").toList)
    AS.parse(tokens.tail, tokens.head.value)(getAgg(tokens.head.nextOp))._2
  }

  def tokenize(input: List[String]): Unparsed = input match {
    case Nil => Nil
    case n :: rest =>
      if (rest.isEmpty) List(Token("", Number(n.toInt)))
      else rest match {
        case "+" :: tail => new Token("+", Number(n.toInt)) :: tokenize(tail)
        case "-" :: tail => new Token("-", Number(n.toInt)) :: tokenize(tail)
        case "*" :: tail => new Token("*", Number(n.toInt)) :: tokenize(tail)
        case "/" :: tail => new Token("/", Number(n.toInt)) :: tokenize(tail)
      }
  }

  case class Number(n: Int) extends Expression
  case class Add(l: Expression, r: Expression) extends Expression
  case class Subtract(l: Expression, r: Expression) extends Expression
  case class Multiply(l: Expression, r: Expression) extends Expression
  case class Divide(l: Expression, r: Expression) extends Expression

  case class Token(nextOp: String, value: Expression)

  object AS extends Expression {
    def parse(unparsed: Unparsed, acc: Expression)(aggregator: (Expression, Expression) => Expression): Parsed = unparsed match {
      case Nil => (Nil, acc)
      case right :: rest => right.nextOp match {
        case "+" => AS.parse(rest, aggregator(acc, right.value))(getAgg("+"))
        case "-" => AS.parse(rest, aggregator(acc, right.value))(getAgg("-"))
        case "*" =>
          val (next, accedMDExpr) = MD.parse(rest, right.value)(getAgg("*"))
          if (next.isEmpty) (Nil, aggregator(acc, accedMDExpr))
          else AS.parse(next.tail, aggregator(acc, accedMDExpr))(getAgg(next.head.nextOp))
        case "/" =>
          val (next, accedDivExp) = MD.parse(rest, right.value)(getAgg("/"))
          if (next.isEmpty) (Nil, aggregator(acc, accedDivExp))
          else AS.parse(next.tail, aggregator(acc, accedDivExp))(getAgg(next.head.nextOp))
        case "" => (Nil, aggregator(acc, right.value))
      }
    }
  }

  object MD extends Expression {
    def parse(unparsed: Unparsed, acc: Expression)(aggregator: (Expression, Expression) => Expression): Parsed = unparsed match {
        case Nil => (Nil, acc)
        case right :: rest => right.nextOp match {
          case "*" => MD.parse(rest, aggregator(acc, right.value))(getAgg("*"))
          case "/" => MD.parse(rest, aggregator(acc, right.value))(getAgg("/"))
          case "" => (Nil, aggregator(acc, right.value))
          case _ => (unparsed, aggregator(acc, right.value))
        }
    }
  }
}

object Test {
  import Calculator._
  def run: Unit = {
    val e1 = "1 + 2"
    val _1 = Calculator.run(e1)
    println(_1.calculate == 3)

    val e2 = "1 * 2"
    val _2 = Calculator.run(e2)
    println(_2.calculate == 2)

    val e3 = "4 / 2"
    val _3 = Calculator.run(e3)
    println(_3.calculate == 2)

    val e4 = "14 - 2"
    val _4 = Calculator.run(e4)
    println(_4.calculate == 12)

    val e5 = "4 - 2 * 5 + 2 / 2"
    val _5 = Calculator.run(e5)
    println(_5.calculate == -5)
  }
}


object Main extends App {
  Test.run
}
