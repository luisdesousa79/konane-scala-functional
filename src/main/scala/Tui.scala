import Konane.Board
import Konane.Coord2D

object Tui {


  def auxTUI(lst: List[Option[Stone.stone]]): String = lst match {
    case Nil => ""           // ✅ base case: lista vazia
    case x :: xs =>
      (x match {
        case Some(Stone.Black) => "B "
        case Some(Stone.White) => "W "
        case Some(Stone.Empty) => ". "
        case None => ". "
      }) + auxTUI(xs)
  }

  def mostrar(board: Board, rows: Int, cols: Int): Unit = mostrarRec(board, rows, cols, 0)

  def mostrarRec(board: Board, rows: Int, cols: Int, currentRow: Int): Unit = currentRow match {
    case r if r == rows => ()
    case r =>
      val linha = (0 until cols).map(y => board.get((r, y))).toList
      println(auxTUI(linha))
      mostrarRec(board, rows, cols, r + 1)
  }

}
