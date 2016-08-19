object Calculator {
  type Unparsed = List[String]
  type Parsed = (Unparsed, Expression)

  abstract class Expression {
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

  def tokenize(input: String): Unparsed = input.split(" ").toList

  case class Number(n: Int) extends Expression
  case class Add(l: Expression, r: Expression) extends Expression
  case class Subtract(l: Expression, r: Expression) extends Expression
  case class Multiply(l: Expression, r: Expression) extends Expression
  case class Divide(l: Expression, r: Expression) extends Expression

  object Number {
    def parse(input: Unparsed): Parsed = (input.tail, Number(input.head.toInt))
  }

  object Multiply {
    def parse(input: Unparsed): Parsed = input match {
      case Nil => (Nil, Number(1))
      case left :: rest =>
        if (!rest.isEmpty) rest match {
          case operator :: tail => operator match {
            case "*" =>
              val (unparsed, parsed) = Multiply.parse(tail)
              (unparsed, Multiply(Number(left.toInt), parsed))

            case "/" =>
              val (unparsed, parsed) = Multiply.parse(tail)
              (unparsed, Divide(Number(left.toInt), parsed))

            case _ => (rest, Number(left.toInt))
          }
        }
        else Number.parse(input)
    }
  }

  object Subtract {
    def parse(input: Unparsed, agg: Expression): Parsed = input match {
      case Nil => (Nil, agg)
      case left :: rest =>
        if (!rest.isEmpty) rest match {
          case operator :: tail => operator match {
            case "+" => Add.parse(tail, Subtract(agg, Number(left.toInt)))

            case "-" => Subtract.parse(tail, Subtract(agg, Number(left.toInt)))

            case _ =>
              val (unparsed, parsed) = Multiply.parse(input)
              val aggregated = Add(parsed, agg)

              if (unparsed.isEmpty) (Nil, aggregated)
              else {
                val op = unparsed.head
                if (op == "+") Add.parse("0" :: unparsed, aggregated)
                else Subtract.parse("0" :: unparsed, aggregated)
              }
          }
        }
        else {
          val (unparsed, parsed) = Number.parse(input)
          (unparsed, Subtract(agg, parsed))
        }
    }
  }

  object Add {
    def parse(input: Unparsed, agg: Expression): Parsed = input match {
      case Nil => (Nil, agg)
      case left :: rest =>
        if (!rest.isEmpty) rest match {
          case operator :: tail => operator match {
            case "+" => Add.parse(tail, Add(agg, Number(left.toInt)))

            case "-" => Subtract.parse(tail, Add(agg, Number(left.toInt)))

            case _ =>
              val (unparsed, parsed) = Multiply.parse(input)
              val aggregated = Add(parsed, agg)

              if (unparsed.isEmpty) (Nil, aggregated)
              else {
                val op = unparsed.head
                if (op == "+") Add.parse("0" :: unparsed, aggregated)
                else Subtract.parse("0" :: unparsed, aggregated)
              }
          }
        }
        else {
          val (unparsed, parsed) = Number.parse(input)
          (unparsed, Add(agg, parsed))
        }
    }
  }
}

object Test {
  import Calculator._
  def run: Unit = {
    val shouldBe11 = "1 + 2 * 3 + 4"
    val _11s = tokenize(shouldBe11)
    val _11exp = Add.parse(_11s, Number(0))._2
    val _11 = _11exp.calculate
    println(shouldBe11)
    println(_11 == 11)
    println(_11exp.print)

    val shouldBe7 = "1 + 2 * 3"
    val _7s = tokenize(shouldBe7)
    val _7exp = Add.parse(_7s, Number(0))._2
    val _7 = _7exp.calculate
    println(shouldBe7)
    println(_7 == 7)
    println(_7exp.print)

    val shouldBe6 = "0 * 1 + 2 * 3"
    val _6s = tokenize(shouldBe6)
    val _6exp = Add.parse(_6s, Number(0))._2
    val _6 = _6exp.calculate
    println(shouldBe6)
    println(_6 == 6)
    println(_6exp.print)

    val shouldBe5 = "6 / 2 + 2 - 0"
    val _5s = tokenize(shouldBe5)
    val exp = Add.parse(_5s, Number(0))._2
    val _5 = exp.calculate
    println(shouldBe5)
    println(_5 == 5)
    println(exp.print)

    val shouldBeneg6 = "0 * 1 - 2 * 3"
    val _neg6s = tokenize(shouldBeneg6)
    val _negexp6s = Add.parse(_neg6s, Number(0))._2
    val _neg6 = _negexp6s.calculate
    println(shouldBeneg6)
    println(_neg6 == -6)
    println(_neg6)
    println(_negexp6s.print)


    println(Add(Number(2), Number(2)).calculate == 4)
    println(Add(Multiply(Number(2), Number(2)), Number(2)).calculate == 6)
    println(Subtract(Multiply(Number(2), Number(2)), Number(2)).calculate == 2)
    println(Divide(Multiply(Number(2), Number(2)), Number(2)).calculate == 2)
  }
}


object Main extends App {
  Test.run
}
