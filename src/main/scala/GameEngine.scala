import Konane.*
import TUI.*
import Konane.GameState
import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}
import GameMode.*
import Phase.{Capturing, InitialRemoval, Playing, SecondRemoval}

object GameEngine:
  @tailrec
  def showMenu(): Unit = { //Usado
    print("1. Iniciar Jogo \n")
    print("2. Sair \n")
    println("Escolha sua opção: ")

    val rand0 = MyRandom(1) //Random Inicial

    getUserInputInt match
      case 1 =>
        val (state, timer, mode , dificuldade , row, columns) = setupOfGame
        val contextoInicial = GameContext(state,Phase.InitialRemoval,None,mode, dificuldade,timer,rand0,Nil, row , columns)  //Aqui irá começar o fluxo do jogo
        gameLoop(contextoInicial)


      case 2 => print("A sair...")
      case _ =>
        println("Opcão Inválida!")
        showMenu()

  }
  @tailrec
  def getUserInputInt: Int = {

    Try(readLine().toInt) match {
      case Success(value) => value
      case Failure(_) =>
        println("Número inválido!")
        getUserInputInt
    }
  }
  @tailrec
  def chooseGame: GameMode = {
    println("Modo de jogo:")
    println("1 - Player vs Player")
    println("2 - Player vs Computador")
    println("3 - Computador vs Computador")
    getUserInputInt match
      case 1 => GameMode.PvP
      case 2 => GameMode.PvC
      case 3 => GameMode.CvC
      case _ => println("Escolha uma opcão válida!")
        chooseGame
  }
  @tailrec
  def chooseDificuldade: Difficulty = {

    println("De acordo com as dificuldade listada abaixo, escolha uma, de acordo com o indice")
    println("1 -> Easy")
    println("2 -> Medio")
    println("3 -> Dificil")

    getUserInputInt match
      case 1 => Difficulty.Easy
      case 2 => Difficulty.Medium
      case 3 => Difficulty.Hard
      case _ =>
        println("Coloque um valor Válido!")
        chooseDificuldade
  }
  def chooseTamanhoTab(): (Int, Int) = {
    println("Vamos escolher o tamanho do Tabuleiro!")
    val largura = lerTamanhoValido("Largura")
    val comprimento = lerTamanhoValido("Comprimento")
    (largura, comprimento)
  }
  @annotation.tailrec
  def lerTamanhoValido(texto: String): Int = {
    println(s"$texto (par entre 6 e 10):")
    val value = getUserInputInt
    if (isValidSize(value)) value
    else {
      println("Valor inválido! O número deve ser par e entre 6 e 10.")
      lerTamanhoValido(texto)
    }
  }
  def isValidSize(n: Int): Boolean =
    n >= 6 && n <= 10 && n % 2 == 0
  @tailrec
  def chooseTimer: Long = {
    println("Escolha o timer (mínimo 1 minuto):")

    val result = Try(getUserInputInt.toLong).toOption

    result match {
      case Some(time) if time >= 1 => time * 60 * 1000 //Estamos a converter para milissegundos

      case _ =>
        println("Valor inválido.")
        chooseTimer
    }
  } //Usado
  def setupOfGame: (GameState, Long, GameMode , Difficulty , Int , Int) = {
    println("Configuarações de jogo ")
    //Vamos escolher aqui o modo de jogo
    val mode_game = chooseGame
    //Escolha de Timer
    val timer = chooseTimer
    //Escolha de Tamanho
    val (rows, columns) = chooseTamanhoTab()
    //InitBoardAqui + Remover as peças Inicias Aqui
    val boardShow = initBoard(rows, columns)
//    println("Eis o Tabuleiro de jogo")
//    printBoard(boardShow)
    //Criar o GameSatet
    val gameSate0 = new GameState(boardShow, Stone.Black, List())
    val dificuldade = chooseDificuldade
    (gameSate0, timer, mode_game, dificuldade, rows, columns)
  }
  //Vamos fazer setup das condições de jogo sendo estas:  Tipo Jogo, Tamanho Tabuleiro , Peças a remover , Tempo Máximo de jogo(timer)
  def showGameOption(): Unit = {
    println("Escolha uma opção abaixo: ")
    println("1 -> Realizar Jogada")
    println("2 -> Undo")
    println("3 -> Reinicia Jogo -> Voltar ao Menu Inicial")
    println("4 -> Sair")
  }
  @tailrec
  def askPieces(validPieces: List[Coord2D]): Coord2D = {
    println("De acordo com as Peças a seguir:")
    printPecas(validPieces) //Função auxiliar que faz print das peças recebidas
    println("Escolha a linha: ")
    val indL = getUserInputInt
    println("Escolha a coluna: ")
    val indC = getUserInputInt
    val coordEscolhida = (indL,indC)
    if validPieces.contains(coordEscolhida) then
      coordEscolhida
    else
      println("Coordenada inválida! Escolha novamente.")
      askPieces(validPieces)

  }
  @tailrec
  def gameLoop(ctx: GameContext): Unit = {
    if isGameOver(ctx.state._1, ctx.state._2, ctx.state._3) && ctx.phase != Phase.InitialRemoval && ctx.phase != Phase.SecondRemoval then
      val Vencedor = switchPlayer(ctx.state._2)
      println(s"Jogo Acabou! Vencedor: $Vencedor")
      showMenu() //Mostramos o Menu Novamente
    else
      println("Tabuleiro de Jogo: ")
      printBoard(ctx.state._1) //Mostramos o tabuleiro.
      println(s"Jogador atual: ${ctx.state._2}") //Indicamos o Jogador Atual.
      showGameOption() //Opções de Jogo -> Realizar Jogada , Undo (voltamos uma jogada Atrás), Reinicar(voltar ao Meuno)
      getUserInputInt match
        case 1 =>
          if isComputerTurn(ctx) then
            println("Computador Vai Jogar!!!!")
            val resultado = JogoCumpter(ctx)
            printMoves(resultado.moves)
            gameLoop(resultado.context)
          else{
            val ActTime = System.currentTimeMillis()
            executarTurnoPlayer(ctx) match
              case Some(newCtx) =>
                if(isTimeExceeded(ActTime,ctx.timerLimit)) then
                  println("Tempo Excedido, o Computador vai jogar Por ti!")
                  val novoctx = JogoCumpter(ctx)
                  println("Jogada Realizada(s): ")
                  printMoves(novoctx.moves)
                  gameLoop(novoctx.context)
                else
                  gameLoop(newCtx)
              case None =>
                gameLoop(ctx)
          }
        case 2 =>
          println("A fazer Undo")
          Konane.undoMove(ctx.history) match {
            case Some((estadoAntigo, remainingHistory)) => //Caso tenhamos alguns estado colocamos o state como estadoAntigo , usamos o remainingHistory ( resto da historia)
              println("Undo realizado com sucesso!!!")
              gameLoop(ctx.copy(state = estadoAntigo, history = remainingHistory, phase = Phase.Playing, selected = None)) //A fase terá de ser Playing , sem nenhuma peça selecionada. Estado InitRemove e SecondRemove não atingiveis usando Undo.
            case None =>
              println("Sem jogadas no histórico para desfazer -> Undo não feito.") //Sem Undo -> Devolvemos Nil
              gameLoop(ctx)
          }
        case 3 =>
          showMenu()
        case 4 =>
          println("A sair...")
        case _ =>
          println("Clique numa opção valida!")
          gameLoop(ctx) //Voltamos ao Inicio.
  }
  def isComputerTurn(ctx: GameContext): Boolean = {
    ctx.mode match
      case PvP => false
      case PvC =>
        ctx.state._2 == Stone.White
      case CvC =>
        true
  }
  def phaseMensagem(ctx: GameContext): String = {
    ctx.phase match
      case InitialRemoval =>
        "Escolha a primeira peça a remover"

      case SecondRemoval =>
        "Escolha a segunda peça adjacente"

      case Playing =>
        ctx.selected match
          case None =>
            "Escolha uma peça para Jogar"

          case Some(piece) =>
            s"Escolha destino para $piece"

      case Capturing =>
        val piece = ctx.selected match
          case None =>
            None
          case Some(piece) =>
            piece
        s"Pode continuar a capturar , seleciona uma peça diferente ou a tua $piece ( Selecionar a tua peça faz com que a captura pare)"
  }
  def turnFinished(oldCtx: GameContext, newCtx: GameContext): Boolean = {
    oldCtx.state._2 != newCtx.state._2 //Aqui vemos quem esta a jogar antes e depois.
  }
  @tailrec
  def executarTurnoPlayer(ctx: GameContext): Option[GameContext] = {
    val validMoves = getValidInteractions(ctx.state , ctx.phase, ctx.selected , ctx.rows , ctx.columns) //Moves Válidos Atualmente -> Na primeira execucão esta nos devolve apenas as posiçoes para onde podemos nos mover.
    val nextCtxOpt = {
      println(phaseMensagem(ctx)) //Demonstramos o Contexto do Jogo Atual. //No caso de ser o Computador a jogar
      playerInteraction(ctx, validMoves) //Se tivermos no Turno do Humano/Player -> playerInteraction
    }
    nextCtxOpt match
      case Some(newCtx) =>
        if turnFinished(ctx, newCtx) then //Caso o turno nao tenha fechado, ou seja o jogador Atual ainda é o Mesmo que entrou vamos para o "Else" no caso em que já mudamos de jogador -> O turno deste Acabou.
          Some(newCtx)
        else
          executarTurnoPlayer(newCtx) //Executamos o turno novamente Aqui -> Fase Capturing Aqui, ou Escolha de Segunda Peça.
      case None =>
        None //Demonstra que ocorreu algum tipo de erro Aqui.
  }
  def playerInteraction(ctx: GameContext, validInteractions: List[Coord2D]): Option[GameContext] = {
    val chosenCoord = askPieces(validInteractions)
    processInteraction(ctx, chosenCoord)
  }














