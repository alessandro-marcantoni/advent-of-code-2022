import utils.Common.readLines

object RockPaperScissors {
    def main(args: Array[String]): Unit = {
        val lines = readLines("rock-paper-scissors.txt")
        println(rounds(lines, myMoves).map(r => r._2.value + 1 + winner(r._1, r._2).value).sum)
        println(rounds(lines, expectedResult).map(r => r._2.value + myMove(r._1, r._2).value + 1).sum)
    }

    sealed class Move(val value: Int)
    case object Rock extends Move(0)
    case object Paper extends Move(1)
    case object Scissors extends Move(2)

    sealed class Result(val value: Int)
    case object Win extends Result(6)
    case object Lose extends Result(0)
    case object Draw extends Result(3)

    def winner(opponentMove: Move, myMove: Move): Result = (opponentMove, myMove) match {
        case (o, m) if o.value == m.value => Draw
        case (o, m) if (o.value - m.value + 3) % 3 == 1 => Lose
        case _ => Win
    }

    def myMove(opponentMove: Move, expectedResult: Result): Move = expectedResult match {
        case Draw => opponentMove
        case Win => new Move((opponentMove.value + 1) % 3)
        case _ => new Move((opponentMove.value + 2) % 3)
    }

    def rounds[T](lines: List[String], interpretation: String => T): List[(Move, T)] =
        lines.map(_.split(" ").toList).map(l => (opponentMoves(l.head), interpretation(l(1))))

    val opponentMoves = Map("A" -> Rock, "B" -> Paper, "C" -> Scissors)
    val myMoves: Map[String, Move] = Map("X" -> Rock, "Y" -> Paper, "Z" -> Scissors)
    val expectedResult: Map[String, Result] = Map("X" -> Lose, "Y" -> Draw, "Z" -> Win)
}
