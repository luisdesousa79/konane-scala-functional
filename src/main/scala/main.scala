@main
def main(): Unit = {

  import Konane.*
  import scala.collection.parallel.immutable.ParMap

  // função random (T1)
  val f = randomMove

  // função para imprimir jogada
  def printMove(player: Stone, before: Board, after: Option[Board], move: Option[Coord2D]): Unit = {
    println("\n----------------------------")
    println("Jogador: " + player)

    move match
      case Some(dest) =>
        after match
          case Some(b) =>

            // elementos que desapareceram do board
            val removed = before.toList.filter(x => !b.toList.contains(x))

            // origem = peça do jogador removida
            val origin = removed.filter(x => x._2 == player).map(x => x._1)

            // capturada = peça adversária removida
            val captured = removed.filter(x => x._2 != player).map(x => x._1)

            println("Origem: " + origin)
            println("Destino: " + dest)
            println("Capturada: " + captured)

          case None =>
            println("Jogada inválida")

      case None =>
        println("Sem jogadas possíveis")

    println("\nTabuleiro:")
    after match
      case Some(b) => printBoard(b)
      case None => printBoard(before)

    println("----------------------------")
  }

  // =========================================
  // TESTE PRINCIPAL (JOGO)
  // =========================================

  println("\n================ JOGO =================")

  val removeA = (2, 2)
  val removeB = (2, 3)
  val open0 = List(removeA, removeB)
  val board0 = initBoard(5, open0)

  println("\n=== BOARD INICIAL ===")
  printBoard(board0)

  val rand0 = MyRandom(1)

  // Jogada 1
  val (b1, r1, open1, move1) =
    playRandomly(board0, rand0, Stone.Black, open0, f)

  printMove(Stone.Black, board0, b1, move1)

  // Jogada 2
  b1 match
    case Some(board1) =>
      val (b2, r2, open2, move2) =
        playRandomly(board1, r1, Stone.White, open1, f)

      printMove(Stone.White, board1, b2, move2)

      // Jogada 3
      b2 match
        case Some(board2) =>
          val (b3, r3, open3, move3) =
            playRandomly(board2, r2, Stone.Black, open2, f)

          printMove(Stone.Black, board2, b3, move3)

          // Jogada 4
          b3 match
            case Some(board3) =>
              val (b4, _, _, move4) =
                playRandomly(board3, r3, Stone.White, open3, f)

              printMove(Stone.White, board3, b4, move4)

            case None => println("Fim do jogo")

        case None => println("Fim do jogo")

    case None => println("Fim do jogo")

}