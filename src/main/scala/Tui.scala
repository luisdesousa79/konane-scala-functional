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

  def mostrar(board: Option[Board], rows: Int, cols: Int): Unit = board match {
    // Cabeçalho
    case Some(a)=>def printHeader(col: Int): Unit = col match {
      case c if c == cols => println()
      case c =>
        val letra = ('A' + c).toChar
        print(s" $letra")
        printHeader(c + 1)
    }

    print("  ")
    printHeader(0)
    mostrarRec(board, rows, cols, 0)
    case none => println("não é valido")
  }



  def mostrarRec(board: Option[Board], rows: Int, cols: Int, currentRow: Int): Unit = currentRow match {
    case r if r == rows => ()
    case r =>
      print(String.format("%2d ", r + 1))
      board match {
        case Some(a)=>
          val linha = (0 until cols).map(y => a.get((r, y))).toList
          println (auxTUI(linha))
          mostrarRec (board, rows, cols, r + 1)
      }
  }

}
