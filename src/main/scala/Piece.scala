sealed trait Color
case object White extends Color
case object Black extends Color

sealed trait PieceType
case object King extends PieceType
case object Queen extends PieceType
case object Rook extends PieceType
case object Bishop extends PieceType
case object Knight extends PieceType
case object Pawn extends PieceType

case class Piece(pieceType: PieceType, color: Color)
