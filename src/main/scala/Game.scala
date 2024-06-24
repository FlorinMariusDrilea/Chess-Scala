class Game {
  private val board = new Board
  private var currentPlayer: Color = White

  private def switchPlayer(): Unit = {
    currentPlayer = if (currentPlayer == White) Black else White
  }

  def play(): Unit = {
    while (true) {
      board.displayBoard()
      println(s"Current Player: ${if (currentPlayer == White) "White" else "Black"}")
      println("Enter move (e.g., e2 e4): ")
      val move = scala.io.StdIn.readLine().split(" ")

      if (move.length == 2) {
        val from = Position(move(0)(0) - 'a', move(0)(1) - '1')
        val to = Position(move(1)(0) - 'a', move(1)(1) - '1')

        if (board.movePiece(from, to)) {
          switchPlayer()
        } else {
          println("Invalid move!")
        }
      } else {
        println("Invalid input!")
      }
    }
  }
}
