import Konane.Board
import Konane.Coord2D

object Tui {


  def auxTUI(lst: List[Option[Stone.stone]]): String = lst match {
    case Nil => ""
    case x :: xs =>
      (x match {
        case Some(Stone.Black) => "B "
        case Some(Stone.White) => "W "
        case Some(Stone.Empty) => ". "
        case None => ". "
      }) + auxTUI(xs)
  }

  def mostrar(board: Board, rows: Int, cols: Int): Unit = {
    // Cabeçalho
    def printHeader(col: Int): Unit = col match {
      case c if c == cols => println()
      case c =>
        val letra = ('A' + c).toChar
        print(s" $letra")
        printHeader(c + 1)
    }

    print("  ")
    printHeader(0)
    mostrarRec(board, rows, cols, 0)
  }

  def mostrarRec(board: Board, rows: Int, cols: Int, currentRow: Int): Unit = currentRow match {
    case r if r == rows => ()
    case r =>
      print(String.format("%2d ", r + 1))
      val linha = (0 until cols).map(y => board.get((r, y))).toList
      println(auxTUI(linha))
      mostrarRec(board, rows, cols, r + 1)
  }

}
