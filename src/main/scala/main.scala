import Konane.{Board, Coord2D, play}
import Stone.{Black, Empty, White}

import scala.collection.parallel.immutable.ParMap

object Main extends App {

  // Tabuleiro 3x3
  val board: Board = ParMap(
    (0, 0) -> Black, (1, 0) -> White, (2, 0) -> Black,
    (0, 1) -> White, (1, 1) -> Black, (2, 1) -> White,

  )

  val lstOpenCoords: List[Coord2D] = List((2, 2),(2,0))

  println("Tabuleiro:")
  Tui.mostrar(Some(board), 3, 3)


  println(s"Posições livres: $lstOpenCoords")

  val rand = MyRandom(42L) // seed fixa para teste previsível

  val result = Konane.playRandomly(board, rand, Black, lstOpenCoords, Konane.randomMove)

  result match {
    case (Some(newBoard), newRand, newLstOpen, Some(move)) =>
      println(s"\nJogada para: $move")
      println("Novo tabuleiro:")
      Tui.mostrar(Some(newBoard), 3, 3)

    case (None, _, _, None) =>
      println("\nSem jogadas válidas!")
  }

  val a = Konane.play(board,Stone.Black,(0,0),(2,0),lstOpenCoords)
  a match{
    case (Some(a),p)=>
      Tui.mostrar(Some(a),3,3)
  }
}