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
  }

  def tokenize(input: String): Unparsed = input.split(" ").toList

  case class Number(n: Int) extends Expression
  case class Add(l: Expression, r: Expression) extends Expression
  case class Subtract(l: Expression, r: Expression) extends Expression
  case class Multiply(l: Expression, r: Expression) extends Expression
  case class Divide(l: Expression, r: Expression) extends Expression

  object Number {
    def parse(input: Unparsed): Parsed =
      (input.tail, Number(input.head.toInt))
  }

  object Multiply {
    def parse(input: Unparsed): Parsed = input match {
      case Nil => (Nil, Number(1))
      case left :: rest =>
      if (!rest.isEmpty) {
        val operator = rest.head
        val tail = rest.tail
        if (operator == "*" || operator ==  "/") {
          val (unparsed, parsed) = Multiply.parse(tail)

          (unparsed, Multiply(Number(left.toInt), parsed))
        }
        else (rest, Number(left.toInt))
      } else Number.parse(input)
    }
  }

  object Add {
    def parse(input: Unparsed, agg: Expression): Parsed = input match {
      case Nil => (Nil, Number(0))
      case left :: rest =>
        if (!rest.isEmpty) {
          val operator = rest.head
          val tail = rest.tail

          if (operator == "*" || operator == "/") {
            val (unparsed, parsed) = Multiply.parse(input)
            val sum = Add(parsed, agg)

            if (!unparsed.isEmpty) Add.parse("0" :: unparsed, sum)
            else (unparsed, sum)
          }
          else {
            val (unparsed, parsed) = Add.parse(tail, agg)
            (unparsed, Add(agg, Add(Number(left.toInt), parsed)))
          }
        } else Number.parse(input)
    }
  }
}

object Test {
  import Calculator._
  def run: Unit = {
    val shouldBe11 = "1 + 2 * 3 + 4"
    val _11s = tokenize(shouldBe11)
    val _11 = Add.parse(_11s, Number(0))._2.calculate
    println(_11 == 11)

    val shouldBe7 = "1 + 2 * 3"
    val _7s = tokenize(shouldBe7)
    val _7 = Add.parse(_7s, Number(0))._2.calculate
    println(_7 == 7)

    val shouldBe6 = "0 * 1 + 2 * 3"
    val _6s = tokenize(shouldBe6)
    val _6 = Add.parse(_6s, Number(0))._2.calculate
    println(_6 == 6)

    val shouldBe2half = "1 / 2 + 2 - 0"
    val _2halfs = tokenize(shouldBe2half)
    val _2half = Add.parse(_2halfs, Number(0))._2.calculate
    println(_2half)
    println(_2half == 2.5)


    println(Add(Number(2), Number(2)).calculate == 4)
    println(Add(Multiply(Number(2), Number(2)), Number(2)).calculate == 6)
    println(Subtract(Multiply(Number(2), Number(2)), Number(2)).calculate == 2)
    println(Divide(Multiply(Number(2), Number(2)), Number(2)).calculate == 2)
  }
}


object Main extends App {
  Test.run
}
