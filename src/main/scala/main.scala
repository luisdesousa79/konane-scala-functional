import Konane.{Board, Coord2D, playRandomly, randomMove, initBoard}
import Konane.Stone.Black
import scala.collection.parallel.immutable.ParMap

import Konane.{Board, Coord2D, playRandomly, randomMove, initBoard}
import Konane.Stone.Black
import scala.collection.parallel.immutable.ParMap

object Main extends App {

  // Define o tamanho do tabuleiro (5x5)
  val n = 5

  // Lista de posições que serão removidas do tabuleiro (a posição central)
  val removed: List[Coord2D] = List((2, 2))

  // Inicializa o tabuleiro com o tamanho definido e remove a posição central
  // O tabuleiro começa com pedras alternadas (Preto, Branco, Preto, Branco...)
  val board = initBoard(n, removed)

  // Cria uma lista com TODAS as coordenadas possíveis do tabuleiro
  // Usa foldLeft para construir a lista linha por linha
  // Exemplo: (0,0), (0,1), (0,2)... até (4,4)
  val allCoords = (0 until n).foldLeft(List.empty[Coord2D]) { (acc, i) =>
    acc ++ (0 until n).map(j => (i, j))  // Para cada linha i, adiciona todas as colunas j
  }

  // Filtra apenas as posições que estão VAZIAS (não contêm pedras)
  // Usa filterNot para pegar as coordenadas que NÃO estão no tabuleiro
  val lstOpenCoords = allCoords.filterNot(board.contains)

  // Cria um gerador de números aleatórios usando o tempo atual como semente
  // Isso garante movimentos diferentes cada vez que o programa roda
  val rand = MyRandom(System.currentTimeMillis())

  // Cria uma instância da interface de texto para mostrar o tabuleiro
  val tui = Tui()

  // Mostra informações iniciais do jogo
  println(s"Tabuleiro ${n}x${n} com remoção em $removed")

  // Exibe o tabuleiro inicial na tela
  // O Some() indica que o tabuleiro existe (não está vazio)
  tui.mostrar(Some(board))

  // Tenta fazer uma jogada aleatória para o jogador PRETO (Black)
  // Recebe:
  // - board: tabuleiro atual
  // - rand: gerador aleatório
  // - Black: jogador que vai jogar
  // - lstOpenCoords: posições vazias disponíveis
  // - randomMove: função que escolhe um movimento aleatório
  val result = playRandomly(board, rand, Black, lstOpenCoords, randomMove)

  // Trata o resultado da jogada usando pattern matching
  result match {
    // CASO 1: Jogada foi executada com sucesso
    case (Some(newBoard), _, _, Some(move)) =>
      println(s"\nJogada: $move")           // Mostra qual foi a jogada
      tui.mostrar(Some(newBoard))          // Mostra o tabuleiro atualizado

    // CASO 2: Não há jogadas válidas disponíveis
    case (None, _, _, None) =>
      println("\nSem jogadas válidas")      // Informa que o jogador não pode jogar
  }
}