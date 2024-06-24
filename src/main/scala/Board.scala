case class Position(x: Int, y: Int) {
  def inBounds: Boolean = x >= 0 && x < 8 && y >=0 && y < 8
}

class Board {
  private val initialSetup: Map[Position, Piece] = Map(
    Position(0, 0) -> Piece(Rook, White),
    Position(1, 0) -> Piece(Knight, White),
    Position(2, 0) -> Piece(Bishop, White),
    Position(3, 0) -> Piece(Queen, White),
    Position(4, 0) -> Piece(King, White),
    Position(5, 0) -> Piece(Bishop, White),
    Position(6, 0) -> Piece(Knight, White),
    Position(7, 0) -> Piece(Rook, White),
    Position(0, 1) -> Piece(Pawn, White),
    Position(1, 1) -> Piece(Pawn, White),
    Position(2, 1) -> Piece(Pawn, White),
    Position(3, 1) -> Piece(Pawn, White),
    Position(4, 1) -> Piece(Pawn, White),
    Position(5, 1) -> Piece(Pawn, White),
    Position(6, 1) -> Piece(Pawn, White),
    Position(7, 1) -> Piece(Pawn, White),
    Position(0, 7) -> Piece(Rook, Black),
    Position(1, 7) -> Piece(Knight, Black),
    Position(2, 7) -> Piece(Bishop, Black),
    Position(3, 7) -> Piece(Queen, Black),
    Position(4, 7) -> Piece(King, Black),
    Position(5, 7) -> Piece(Bishop, Black),
    Position(6, 7) -> Piece(Knight, Black),
    Position(7, 7) -> Piece(Rook, Black),
    Position(0, 6) -> Piece(Pawn, Black),
    Position(1, 6) -> Piece(Pawn, Black),
    Position(2, 6) -> Piece(Pawn, Black),
    Position(3, 6) -> Piece(Pawn, Black),
    Position(4, 6) -> Piece(Pawn, Black),
    Position(5, 6) -> Piece(Pawn, Black),
    Position(6, 6) -> Piece(Pawn, Black),
    Position(7, 6) -> Piece(Pawn, Black)
  )

  private var boardState: Map[Position, Piece] = initialSetup

  def getPieceAt(position: Position): Option[Piece] = boardState.get(position)

  def movePiece(from: Position, to: Position): Boolean = {
    (boardState.get(from), boardState.get(to)) match {
      case (Some(piece), Some(destinationPiece)) if to.inBounds && isValidMove(piece, from, to) && destinationPiece.color != piece.color =>
        boardState -= from
        boardState += (to -> piece)
        true
      case (Some(piece), None) if to.inBounds && isValidMove(piece, from, to) =>
        boardState -= from
        boardState += (to -> piece)
        true
      case _ => false
    }
  }

  private def isValidMove(piece: Piece, from: Position, to: Position): Boolean = {
    def isPathClear(from: Position, to: Position): Boolean = {
      val xDirection = if (to.x - from.x > 0) 1 else if (to.x - from.x < 0) -1 else 0
      val yDirection = if (to.y - from.y > 0) 1 else if (to.y - from.y < 0) -1 else 0
      var current = Position(from.x + xDirection, from.y + yDirection)
      while (current != to) {
        if (boardState.contains(current)) return false
        current = Position(current.x + xDirection, current.y + yDirection)
      }
      true
    }

    piece.pieceType match {
      case Pawn =>
        val direction = if (piece.color == White) 1 else -1
        val startRow = if (piece.color == White) 1 else 6
        val forwardMove = (from.x == to.x) && (to.y == from.y + direction)
        val initialDoubleMove = (from.x == to.x) && (from.y == startRow) && (to.y == from.y + 2 * direction)
        val captureMove = (Math.abs(from.x - to.x) == 1) && (to.y == from.y + direction) && boardState.contains(to) && boardState(to).color != piece.color
        forwardMove || initialDoubleMove || captureMove

      case Knight =>
        val dx = Math.abs(to.x - from.x)
        val dy = Math.abs(to.y - from.y)
        (dx == 2 && dy == 1) || (dx == 1 && dy == 2)

      case Bishop =>
        Math.abs(from.x - to.x) == Math.abs(from.y - to.y) && isPathClear(from, to)

      case Rook =>
        (from.x == to.x || from.y == to.y) && isPathClear(from, to)

      case King =>
        val dx = Math.abs(from.x - to.x)
        val dy = Math.abs(from.y - to.y)
        (dx <= 1 && dy <= 1)

      case Queen =>
        (from.x == to.x || from.y == to.y || Math.abs(from.x - to.x) == Math.abs(from.y - to.y)) && isPathClear(from, to)


      case _ => false
    }
  }

  def displayBoard(): Unit = {
    val characters = "   a  b  c  d  e  f  g  h"
    println(characters)
    println("   ----------------------")
    for (y <- 7 to 0 by -1) {
      print(s"${y + 1}  ")
      for (x <- 0 until 8) {
        val piece = getPieceAt(Position(x, y)).map {
          case Piece(King, White)   => "♔ "
          case Piece(Queen, White)  => "♕ "
          case Piece(Rook, White)   => "♖ "
          case Piece(Bishop, White) => "♗ "
          case Piece(Knight, White) => "♘ "
          case Piece(Pawn, White)   => "♙ "
          case Piece(King, Black)   => "♚ "
          case Piece(Queen, Black)  => "♛ "
          case Piece(Rook, Black)   => "♜ "
          case Piece(Bishop, Black) => "♝ "
          case Piece(Knight, Black) => "♞ "
          case Piece(Pawn, Black)   => "♟ "
        }.getOrElse(". ")
        print(s"$piece ")
      }
      println(s"${y + 1}")
    }
    println("   ----------------------")
    println(characters)
  }

}
