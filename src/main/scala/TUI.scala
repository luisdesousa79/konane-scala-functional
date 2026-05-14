

import scala.collection.parallel.immutable.ParMap
import scala.annotation.tailrec

import Konane.*

object TUI {

  import scala.annotation.tailrec
//T7
  // Função principal do menu - mostra as opções iniciais
  def menu(): Unit = {
    println("\n=== MENU DE JOGO ===")
    println("1. Jogar ")
    println("Escolha uma opcao:")
    val userInput = IO_Utils.getUserInput()

    userInput match {
      case "1" =>
        // Configuração do jogo (dificuldade, tempo, tamanho)
        println("\n=== CONFIGURACAO DE JOGO ===")
        println("Dificuldade:")
        println("1 - Facil")
        println("2 - Normal")
        val dificuldade = IO_Utils.getUserInput().toInt
        println("Tempo maximo por jogada (segundos):")
        val tempoMaximo = IO_Utils.getUserInput().toInt
        println("Dimensao do tabuleiro (ex: 5 para 5x5):")
        val dimensao = IO_Utils.getUserInput().toInt

        // Cria o tabuleiro inicial (remove a posicao central)
        val removida = (dimensao / 2, dimensao / 2)
        val boardInicial = Konane.initBoard(dimensao, List(removida))
        val openIniciais = calcularPosicoesVazias(boardInicial, dimensao)
        val rand = MyRandom(System.currentTimeMillis())

        // Estado inicial e historico (para permitir reiniciar)
        val estadoInicial: GameState = (boardInicial, Stone.Black, openIniciais)
        val history: GameHistory = List(estadoInicial)

        // Inicia o jogo
        loopJogo(boardInicial, openIniciais, Stone.Black, rand, history,
          dificuldade, tempoMaximo, dimensao)

      case _ => println("Opcao invalida!"); menu()
    }
  }

  // Função recursiva que mantém o jogo rodando
  @tailrec
  def loopJogo(
                board: Board, // Tabuleiro atual
                lstOpenCoords: List[Coord2D], // Posições vazias
                currentPlayer: Stone, // Quem joga agora (Preto ou Branco)
                rand: MyRandom, // Gerador aleatório
                history: GameHistory, // Historico de estados (para reiniciar)
                dificuldade: Int, // 1-Facil, 2-Normal
                tempoMaximo: Int, // Limite de tempo por jogada
                dimensao: Int // Tamanho do tabuleiro
              ): Unit = {

    printBoard(board) // Mostra o tabuleiro
    println(s"\nJogador: ${if (currentPlayer == Stone.Black) "PRETO" else "BRANCO"}")

    // Menu de opções durante o jogo
    println("\n--- OPCOES ---")
    println("1. Fazer jogada")
    println("2. Reiniciar jogo")
    println("3. Sair do jogo")
    println("4. desfazer jogada")
    print("Escolha: ")

    scala.io.StdIn.readLine() match {

      case "1" => // Fazer jogada
        if (Konane.isGameOver(board, currentPlayer, lstOpenCoords)) {
          // Se acabou, mostra vencedor e volta ao menu
          println(s"\nFIM DE JOGO! ${if (currentPlayer == Stone.Black) "BRANCO" else "PRETO"} venceu!")
          println("\nPressione ENTER para voltar ao menu...")
          scala.io.StdIn.readLine()
          menu()
        } else {
          // Função que processa a jogada do humano
          def fazerJogadaHumano(
                                 b: Board, open: List[Coord2D], player: Stone, r: MyRandom, hist: GameHistory
                               ): (Board, List[Coord2D], Stone, MyRandom, GameHistory) = {

            val startTime = System.currentTimeMillis()
            // Guarda estado atual no historico (para poder reiniciar depois)
            val estadoAntes: GameState = (b, player, open)
            val novaHistory = Konane.salvarEstadoAtual(estadoAntes, hist)

            // Pede coordenadas da origem
            print("Digite ORIGEM (linha coluna): ")
            val fromInput = scala.io.StdIn.readLine().trim.split("\\s+")
            val fromCoord = (fromInput(0).toInt - 1, fromInput(1).toInt - 1)

            // Pede coordenadas do destino
            print("Digite DESTINO (linha coluna): ")
            val toInput = scala.io.StdIn.readLine().trim.split("\\s+")
            val toCoord = (toInput(0).toInt - 1, toInput(1).toInt - 1)

            // Verifica se tempo acabou
            if (Konane.isTimeExceeded(startTime, tempoMaximo * 1000L)) {
              println("TEMPO ESGOTADO!")
              (b, open, player, r, novaHistory)
            } else {
              // Tenta executar a jogada
              val (opBoard, novaLista) = Konane.play(b, player, fromCoord, toCoord, open)
              opBoard match {
                case Some(tabuleiroFinal) =>
                  println("Jogada do jogador feita.")
                  // Vez do computador
                  val adversario = if (player == Stone.Black) Stone.White else Stone.Black
                  val estadoAntesCPU: GameState = (tabuleiroFinal, player, novaLista)
                  val histCPU = Konane.salvarEstadoAtual(estadoAntesCPU, novaHistory)
                  val (newBoard, newRand, newCoords, _) = Konane.jogarComDificuldade(
                    dificuldade, tabuleiroFinal, r, adversario, novaLista
                  )
                  newBoard match {
                    case Some(nb) =>
                      println(s"Computador (${if (dificuldade == 1) "Facil" else "Normal"}) jogou.")
                      (nb, newCoords, Stone.Black, newRand, histCPU)
                    case None =>
                      (tabuleiroFinal, novaLista, Stone.Black, r, histCPU)
                  }
                case None =>
                  println("Jogada invalida. Tente novamente.")
                  fazerJogadaHumano(b, open, player, r, novaHistory)
              }
            }
          }

          // Executa a jogada e obtem novo estado
          val (novoBoard, novoOpen, novoPlayer, novoRand, novoHistory) =
            fazerJogadaHumano(board, lstOpenCoords, currentPlayer, rand, history)

          // Continua o jogo com o novo estado
          loopJogo(novoBoard, novoOpen, novoPlayer, novoRand, novoHistory,
            dificuldade, tempoMaximo, dimensao)
        }

      case "2" => // Reiniciar jogo
        println("\n=== REINICIANDO JOGO ===")
        Konane.reiniciar(history) match {
          case Some(estadoInicialRestaurado) =>
            val (b, p, o) = estadoInicialRestaurado
            val novaHistory = List(estadoInicialRestaurado)
            println("Jogo reiniciado com sucesso!")
            loopJogo(b, o, p, rand, novaHistory, dificuldade, tempoMaximo, dimensao)
          case None =>
            println("Nao foi possivel reiniciar.")
            menu()
        }

      case "3" => // Sair do jogo
        println("Saindo do jogo...")
        menu()

      case "4" => //desfazer a jogada
        Konane.undoMove(history)
        loopJogo(board, lstOpenCoords, currentPlayer, rand, history,
          dificuldade, tempoMaximo, dimensao)

      case _ => // Opção inválida
        println("Opcao invalida!")
        loopJogo(board, lstOpenCoords, currentPlayer, rand, history,
          dificuldade, tempoMaximo, dimensao)


    }
  }
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
