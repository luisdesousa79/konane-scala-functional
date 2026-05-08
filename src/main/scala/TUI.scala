import com.sun.source.tree.WhileLoopTree

import scala.collection.parallel.immutable.ParMap
import scala.annotation.tailrec

import Konane.*

object TUI {
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
  def printHeader(size: Int): Unit = {
    @tailrec
    def loop(col: Int): Unit =
      if col < size then //basicamente vamos percorrer as letras de acordo com o tamanho do tabuleiro.
        print(s"${('A' + col).toChar} ")
        loop(col + 1)
      else println()

    print("  ")
    loop(0)
  }

  // Gera uma linha
  def printRow(board: Board, row: Int, size: Int): Unit = {
    @tailrec
    def loop(col: Int): Unit =
      if col < size then
        val celula = getCelula(board, (row, col)) //aqui vamos percorrer os outros elementos da coluna, ou seja já estamos na linha.
        print(s"$celula ")
        loop(col + 1)
      else println()

    print(s"$row ")
    loop(0)
  }

  // Função principal que vai chamar as outras auxiliares
  def printBoard(board: Board): Unit = {
    val size = boardSize(board) //calculamos o tamanho da board.

    printHeader(size) //fazemos print do header(cabecalho) como na imagem do enunciado

    @tailrec
    def loop(row: Int): Unit =
      if row < size then //aqui percorremos linha a linha, as outras funções vao percorrer coluna a coluna.
        printRow(board, row, size)
        loop(row + 1)

    loop(0)
  }
}
