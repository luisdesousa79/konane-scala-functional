import Konane.Board
import Konane.Coord2D

object Tui {


  def auxTUI(lst: List[Option[Stone.stone]]): String = lst match { // essa é a função que pega cada pedra numa lista, e transforma em uma letra
    case Nil => ""
    case x :: xs =>
      (x match {
        case Some(Stone.Black) => "B "
        case Some(Stone.White) => "W "
        case Some(Stone.Empty) => ". "
        case None => ". "
      }) + auxTUI(xs)
  }

  def mostrar(board: Option[Board], rows: Int, cols: Int): Unit = board match { // recebe um board, e as suas dimenções, depois de printar o cabeçalho chama o mostrarRec
    // Cabeçalho
    case Some(a)=>def printHeader(col: Int): Unit = col match {
      // CASO BASE: quando chega ao fim
      case c if c == cols => println()

      // CASO RECURSIVO: ainda há colunas para imprimir
      case c =>
        val letra = ('A' + c).toChar  // 0->'A', 1->'B', 2->'C', etc. // é só lembra
        print(s" $letra")              // imprime " A", " B", " C", etc.
        printHeader(c + 1)             // chama para a próxima coluna
    }


    print("  ")
    printHeader(0)
    mostrarRec(board, rows, cols, 0)
    case none => println("não é valido")
  }



  def mostrarRec(board: Option[Board], rows: Int, cols: Int, currentRow: Int): Unit = currentRow match {
    case r if r == rows => ()
    case r =>
      print(String.format("%2d ", r + 1)) //coloca os numeros a frente de cada linha
      board match {
        case Some(a)=>
          val linha = (0 until cols).map(y => a.get((r, y))).toList
          println (auxTUI(linha))
          mostrarRec (board, rows, cols, r + 1)
      }
  }

}
