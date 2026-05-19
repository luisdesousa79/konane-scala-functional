import scala.annotation.tailrec
import Konane.*


object TUI:

  @tailrec
  def printPecas(validPieces: List[Coord2D]): Unit =
    validPieces match {
      case Nil => ()
      case (x, y) :: tail =>
        println(s"Posicão: ($x, $y)")
        printPecas(tail)
    }

  @tailrec
  def printMoves(moves: List[(Coord2D, Coord2D)]): Unit =

    moves match {
      case Nil => ()
      case (from, to) :: tail =>
        println(s"$from -> $to")
        printMoves(tail)
    }  //Usado apenas para demonstrar as jogadas do Computador.
  //T4
  // Converte Stone para Char
  def stoneToChar(stone: Stone): Char = stone match {
    case Stone.Black => 'B'
    case Stone.White => 'W'
  }

  // Buscamos na board, passando a board (tabuleiro) e depois a coordenada.
  def getCelula(board: Board, coord: Coord2D): Char = {
    board.get(coord) match
      case Some(stone) => stoneToChar(stone)
      case None => '.'
  }

  // Descobre tamanho máximo assumindo que o é tabuleiro quadrado
  def boardSize(board: Board): Int = {
    board.keys.map((r, c) => math.max(r, c)).max + 1 //assumimos também que aqui as posições esta bem ordenadas, ou seja , não vamos ter um Black, (10,10), enquanto o tamanho do tabuleiro é 5 x 5.
  }

  // Gera header (A B C D ...) , como no exemplo do enunciado
  def printHeader(cols: Int): Unit = {
    @tailrec
    def loop(col: Int): Unit =
      if col < cols then
        print(s"${('A' + col).toChar} ")
        loop(col + 1)
      else println()

    print("  ")
    loop(0)
  }


  // Gera uma linha
  def printRow(board: Board, row: Int, cols: Int): Unit = {
    @tailrec
    def loop(col: Int): Unit =
      if col < cols then
        val celula = getCelula(board, (row, col))
        print(s"$celula ")
        loop(col + 1)
      else println()

    print(s"$row ")
    loop(0)
  }

  // Função principal que vai chamar as outras auxiliares
  def printBoard(board: Board): Unit = {
    val rows = board.keys.map(_._1).max + 1
    val cols = board.keys.map(_._2).max + 1

    printHeader(cols)

    @tailrec
    def loop(row: Int): Unit =
      if row < rows then
        printRow(board, row, cols)
        loop(row + 1)

    loop(0)
  }

