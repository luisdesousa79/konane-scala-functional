import Difficulty.{Easy, Hard, Medium}
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
        val (state, timer, mode , dificuldade) = setupOfGame
        val contextoInicial = GameContext(state,Phase.InitialRemoval,None,mode, dificuldade,timer,rand0,Nil)  //Aqui irá começar o fluxo do jogo
        gameLoop4(contextoInicial)


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
  } //Usado

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
  } //Usado

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
  } //Usado

  def difficultyLimit(diff: Difficulty): Int =
    diff match
      case Easy => 0
      case Medium => 1
      case Hard => Int.MaxValue


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


  def chooseTamanhoTab: (Int, Int) = {
    println("Vamos escolher o tamanho do Tabuleiro!")
    println("Largura:")
    val largura = getUserInputInt

    //Aplicar restriçoes de Tamanho Aqui. Fazer Depois

    print("Comprimento:")
    val comprimento = getUserInputInt
    (largura, comprimento)
  } //Usada na escolha do Tamanho -> Implementar Restrições de Tamanho -> Tamanho Minimo e Máximo.

  //Errado
  def pecasRemover2(row: Int, cols: Int): List[Coord2D] = { //Apenas usado no Controoller Game.-> GUI.
    List((0,0), (row -1,cols -1))
  }




  def setupOfGame: (GameState, Long, GameMode , Difficulty) = {
    println("Configuarações de jogo ")

    //Vamos escolher aqui o modo de jogo
    val mode_game = chooseGame

    //Escolha de Timer
    val timer = chooseTimer

    //Escolha de Tamanho
    val (lagura, comprimento) = chooseTamanhoTab


    //InitBoardAqui + Remover as peças Inicias Aqui
    val boardShow = initBoard(lagura, comprimento)
    println("Eis o Tabuleiro de jogo")
    printBoard(boardShow)



    //Criar o GameSatet
    val gameSate0 = new GameState(boardShow, Stone.Black, List())

    val dificuldade = chooseDificuldade


    (gameSate0, timer, mode_game, dificuldade)
  } //Usada
  //Vamos fazer setup das condições de jogo sendo estas:  Tipo Jogo, Tamanho Tabuleiro , Peças a remover , Tempo Máximo de jogo(timer)


  def showGameOption(): Unit = {
    println("Escolha uma opção abaixo: ")
    println("1 -> Realizar Jogada")
    println("2 -> Undo") //Susceptivel a alterações, só faz sentido ter undo no caso do jogo já estar a rodar.
    println("3 -> Reinicia Jogo -> Voltar ao Menu Inicial")
    println("4 -> Sair")
  }

  @tailrec
  def askPieces(validPieces: List[Coord2D]): Coord2D = {

    println("De acordo com as Peças a seguir:")

    validPieces.foreach((x, y) => println(s"Position: ($x , $y)")) //Mudar!

    println("Escolha a linha: ")
    val indL = getUserInputInt
    println("Escolha a coluna: ")
    val indC = getUserInputInt
    val coordEscolhida = (indL,indC)

    validPieces.contains(coordEscolhida) match
      case true =>
        coordEscolhida

      case false =>
        println("Coordenada inválida! Escolha novamente.")
        askPieces(validPieces)

  }

  def PlayerMove(state: GameState): GameState = {

    println(s"Peça a movimentar: ${state._2} ")

    //Position Validas -> Fazer display das mesmas
    val listPlayablePieces = listPlayablePositions(state._1, state._2,state._3)

    val coordFroomPiece = askPieces(listPlayablePieces) //Pegamos aqui a peça a jogar

    val PossiveisDestion = listValidDestinations(state._1, state._2,state._3) //Pegamos aqui todas as casas livres, esta função faz return das casas livres de acordo com o player

    val DestinoPossiveis = PossiveisDestion.filter(coordTo => isValidPlay(state._1, state._2, coordFroomPiece , coordTo, state._3)) //filtro de modo a obter apenas as casas para onde podemos jogar de acordo coma nossa peça

    val coordToPiece = askPieces(DestinoPossiveis) //Peça para onde vamos jogar.

    //Vamos agora fazer a jogada:

    val result = play(state._1, state._2 , coordFroomPiece , coordToPiece , state._3) //Result -> Nova Board e Posições Abertas


    //(newBoard , switchPlayer(state._2) , newListOpnes)
    result match
      case (Some(newBoard), newOpenCoords) =>
        val (finalBoard, finalOpenCoords) = continuePlayerCaptures(newBoard, state._2, coordToPiece, newOpenCoords)
        (finalBoard, switchPlayer(state._2), finalOpenCoords)


      case (None, _) =>
        println("Jogada inválida!")
        state //Aqui se calhar poderiamos repetir a jogada...

  } //Usado na versão Anterior!

  @tailrec
  def continuePlayerCaptures(board: Board, player: Stone, currentPos: Coord2D, openCoords: List[Coord2D]): (Board, List[Coord2D]) = {
    // Descobrir próximos movimentos válidos
    val nextDestinations = validDestinationsFromPiece(board, player, currentPos, openCoords)
    // Se não houver mais jogadas possíveis
    if nextDestinations.isEmpty then
      (board, openCoords)
    else
      println(s"Podes continuar a capturar com a peça em $currentPos")
      println("Querem Continuar a jogar?")
      println("1 -> Sim")
      println("2 -> Não")
      getUserInputInt match
        case 1 =>
          println(s"Destinos possíveis: $nextDestinations")
          // Jogador escolhe próximo destino
          val nextCoordTo = askPieces(nextDestinations)

          // Executa nova captura
          val result = play(board, player, currentPos, nextCoordTo, openCoords)

          result match
            case (Some(newBoard), newOpenCoords) =>
              // Continua recursivamente
              continuePlayerCaptures(newBoard, player, nextCoordTo, newOpenCoords)
            case (None, _) =>
              println("Jogada inválida!")
              // Mantém estado atual
              (board, openCoords)

        case 2 => (board, openCoords)

        case _ =>
          println("Escolha uma opção VALIDA!")
          continuePlayerCaptures(board , player , currentPos, openCoords)
  } //Usado na versão Anterior





  @tailrec
  def gameLoop(state: GameState, history: GameHistory, timerLimit: Long, mode: GameMode, r: => MyRandom , diff: => Difficulty ): Unit = {

    println("Tabuleiro de Jogo: ")
    printBoard(state._1) //Print da Board
    println("")

    //Aqui basicamente vamos ver se alguem já ganhou. De acordo com o stone Atual.
    if isGameOver(state._1,state._2,state._3) then
      val winner = switchPlayer(state._2)
      println("Game Over!!")
      println(s"Jogador $winner ganhou")
      showMenu()
    else

      showGameOption()

      getUserInputInt match
        case 1 => //Aqui vai depender do tipo de jogo!
          val timeInit = System.currentTimeMillis() //Começamos a contar o tempo

          val (newState , rp ) = mode match //De acordo com o modo de jogo escolhido vamos realizar um tipo de jogada, aqui vamos pegar o novoEstado, e o novo (caso seja preciso) Random

            case GameMode.PvP =>
              println("Jogo Player vs Player")
              val estado = PlayerMove(state) //Vamos realizar a jogada //PlayerMove vai devolver o novo estado de jogo.

              (estado,r)

              //Vamos ter uma funcao para este tipo de jogo
            case GameMode.PvC => //Aqui temos que ter atenção que player está associado cada peça!
              println("Jogo Player vs Computer")
              if state._2 == Stone.Black then
                val estado = PlayerMove(state)
                (estado, r)
              else
                val (result, newR , jogadasFeitas) = ComputerMove(state , diff, r)
                print(jogadasFeitas)
                (result,newR)

            case GameMode.CvC =>
              println("Computer vs Computer")
              val (result, newR , jogadasFeitas) = ComputerMove(state , diff, r) //Aqui é sempre Computer Move.
              print(jogadasFeitas)

              (result,newR)

//          //Calculo de tempo excedido.
//          isTimeExceeded(timeInit, timerLimit) match
//            case true => println("Aqui vamos ter de fazer uma nova jogaga")
//            case _ => println("Jogada feita a tempo")

          val newHistory = state :: history
          gameLoop( newState , newHistory , timerLimit, mode, rp , diff)

        case 2 =>
          undoMove(history) match
            case Some((estadoAnrigo, restoHistoria)) =>
              println("Undo realizado!")
              gameLoop(estadoAnrigo, restoHistoria, timerLimit, mode, r , diff)
            case None =>
              println("Sem jogadas para desfazer!")
              gameLoop(state, history, timerLimit, mode, r , diff)




        case 3 =>
          val (newState, newTimer, newMode , diff2) = setupOfGame
          gameLoop(newState,Nil ,newTimer, newMode, r , diff2)

        case 4 =>
          println("A sair...")

        case _ =>
          println("Opção Inválida ")
          gameLoop(state,history,timerLimit, mode , r , diff)


  } //Usado na versão Anterior.



  //////////////////////  Atual Aqui.


  def gameLoop4(ctx: GameContext): Unit = {

    printBoard(ctx.state._1) //Mostramos o tabuleiro.
    println(s"Jogador atual: ${ctx.state._2}") //Indicamos o Jogador Atual.

    if (isGameOver(ctx.state._1, ctx.state._2, ctx.state._3) && ctx.phase != Phase.InitialRemoval && ctx.phase != Phase.SecondRemoval) then
      val Vencedor = switchPlayer(ctx.state._2)
      println(s"Jogo Acabou! Vencedor: $Vencedor")
      showMenu() //Mostramos o Menu Novamente
    else
      showGameOption() //Opções de Jogo -> Realizar Jogada , Undo (voltamos uma jogada Atrás), Reinicar(voltar ao Meuno)
      getUserInputInt match

        case 1 => //Realizar jogada
          executeTurn(ctx) match //Executa-mos turno de jogada, ou seja Jogador Escolhe uma peça , Escolhe outra e fazemos jogada.
            case Some(newCtx) => //newCtx -> Novo contexto de jogo.
              gameLoop4(newCtx)  //Loop de jogo -> Outro Player Irá jogar.

            case None =>
              println("Erro na jogada") //Erro na jogada , voltamos ao contexto Atual -> Não deve chegar Aqui
              gameLoop4(ctx)

        case 2 =>
          println("A fazer Undo")
          Konane.undoMove(ctx.history) match {
            case Some((estadoAntigo, remainingHistory)) => //Caso tenhamos alguns estado colocamos o state como estadoAntigo , usamos o remainingHistory ( resto da historia)
              println("Undo realizado com sucesso!!!")
              gameLoop4(ctx.copy(state = estadoAntigo, history = remainingHistory, phase = Phase.Playing, selected = None)) //A fase terá de ser Playing , sem nenhuma peça selecionada. Estado InitRemove e SecondRemove não atingiveis usando Undo.
            case None =>
              println("Sem jogadas no histórico para desfazer -> Undo não feito.") //Sem Undo -> Devolvemos Nil
              gameLoop4(ctx)
          }

        case 3 =>
          showMenu()
        case 4 =>
          println("A sair...")

        case _ =>
          println("Clique numa opção valida!")
          gameLoop4(ctx) //Voltamos ao Inicio.
  } //gameLoop Atual !



  def isComputerTurn(ctx: GameContext): Boolean = {
    ctx.mode match
      case PvP => false
      case PvC =>
        ctx.state._2 == Stone.White
      case CvC =>
        true
  }

  def phaseMessage(ctx: GameContext): String = {
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
        s"Pode continuar a capturar , seleciona uma peça diferente ou a tua ${piece} ( Selecionar a tua peça faz com que a captura pare)"

  }


  def turnFinished(oldCtx: GameContext, newCtx: GameContext): Boolean = {
    oldCtx.state._2 != newCtx.state._2 //Aqui vemos quem esta a jogar antes e depois.
  }

  @tailrec
  def executeTurn(ctx: GameContext): Option[GameContext] = {


    val validMoves = getValidInteractions(ctx.state , ctx.phase, ctx.selected) //Moves Válidos Atualmente -> Na primeira execucão esta nos devolve apenas as posiçoes para onde podemos nos mover.


    val nextCtxOpt =
      if isComputerTurn(ctx) then
        println(s"Computador vai jogar , Peça do Mesmo; ${ctx.state._2} ")
        computerInteraction(ctx, validMoves) //Se estivermos no Turno de Computador -> ComputerInteraction.
      else
        println(phaseMessage(ctx)) //Demonstramos o Contexto do Jogo Atual. //No caso de ser o Computador a jogar -> Não faz sentido
        //Aqui poderiamos perguntar se continua ou não
        //Case Capturing. Caso sim fazemos uma forma de mandar a peça para o playerIntercation.
        playerInteraction(ctx, validMoves) //Se tivermos no Turno do Humano/Player -> playerInteraction

    nextCtxOpt match

      case Some(newCtx) =>
        if turnFinished(ctx, newCtx) then //Caso o turno nao tenha fechado, ou seja o jogador Atual ainda é o Mesmo que entrou vamos para o "Else" no caso em que já mudamos de jogador -> O turno deste Acabou.
          Some(newCtx)
        else
          executeTurn(newCtx) //Executamos o turno novamente Aqui -> Fase Capturing Aqui, ou Escolha de Segunda Peça.

      case None =>
        None //Demonstra que ocorreu algum tipo de erro Aqui.
  }

  
  def playerInteraction(ctx: GameContext, validInteractions: List[Coord2D]): Option[GameContext] = {
    //Ou aqui escolhemos para ele parar ou não.
    val chosenCoord = askPieces(validInteractions)
    processInteraction(ctx, chosenCoord)
  }

  //Limitar aqui de acordo com a dificuldade do jogo.
  def computerInteraction(ctx: GameContext, validInteractions: List[Coord2D]): Option[GameContext] = { //Usar a dificuldade Aqui, Falta Implementar Isso.
    val (chosenMove, newRand) = randomMove(validInteractions, ctx.random) //Escolhe uma peça de forma aleatoria, de acordo com o validIntercations
    val updatedCtx = ctx.copy(random = newRand) //Uptade do contexto, Apenas Mudamos o Random -> Obrigatório.
    processInteraction(updatedCtx, chosenMove) //Usamos o processador de Interações. Este devolve o novo estado.
  }












