import Difficulty.{Easy, Hard, Medium}
import Konane.*
import TUI.*
import Konane.GameState

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}
import GameMode.*

object GameEngine:

  @tailrec
  def showMenu(): Unit = {
    print("1. Iniciar Jogo \n")
    print("2. Sair \n")
    println("Escolha sua opção: ")

    val rand0 = MyRandom(1) //Random Inicial

    getUserInputInt match
      case 1 =>
        val (state, timer, mode , dificuldade) = setupOfGame
        gameLoop(state,Nil,timer,mode, rand0 , dificuldade) //Aqui irá começar o fluxo do jogo

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
  }


  def chooseTamanhoTab: (Int, Int) = {

    println("Vamos escolher o tamanho do Tabuleiro!")
    println("Largura:")
    val largura = getUserInputInt

    //Aplicar restriçoes de Tamanho Aqui. Fazer Depois

    print("Comprimento:")
    val comprimento = getUserInputInt
    (largura, comprimento)
  }

  def possibleInitialRemovals(rows: Int, cols: Int): List[(Coord2D, Coord2D)] = {
    List(((0, cols - 2), (0, cols - 1)), ((rows / 2, cols / 2 - 1), (rows / 2, cols / 2)), ((rows - 1, 0), (rows - 1, 1)))
    //Devolvemos uma lista de conjunto de posições livres. Tendo em conta que as regras konane apenas permitem remover no meio e no canto superior direito , inferior esquerdo.
  }

  //Tail recursive Modificar
  @tailrec
  def chooseInitPosToRemove(rows: Int, cols: Int): List[Coord2D] = {
    val options = possibleInitialRemovals(rows, cols)

    println("Escolha o conjunto de posições iniciais a remover:")
    println(s"1 -> ${options.head}")
    println(s"2 -> ${options(1)}")
    println(s"3 -> ${options(2)}")

    val escolha = getUserInputInt

    escolha match {
      case 1 => List(options.head._1, options.head._2)
      case 2 => List(options(1)._1, options(1)._2)
      case 3 => List(options(2)._1, options(2)._2)
      case _ => println("Opção inválida.")
        chooseInitPosToRemove(rows, cols)
    }
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

    //Escolher Peças a serem removidas (as duas primeiras)
    val removed = chooseInitPosToRemove(lagura, comprimento)

    //val board0 = removePecas(boardShow, removed) //Remover as peças

    //Criar o GameSatet
    val gameSate0 = new GameState(removePecas(boardShow, removed), Stone.Black, removed) //Podemos Melhorar , no caso de ser PvP ou PvC a pessoa poder escolher a sua peça (Preta ou Branca)



    val dificuldade = chooseDificuldade


    (gameSate0, timer, mode_game, dificuldade)
  }
  //Vamos fazer setup das condições de jogo sendo estas:  Tipo Jogo, Tamanho Tabuleiro , Peças a remover , Tempo Máximo de jogo(timer)

  def switchPlayer(player: Stone): Stone =
    player match
      case Stone.Black => Stone.White
      case Stone.White => Stone.Black

  def showGameOption(): Unit = {
    println("Escolha uma opção abaixo: ")
    println("1 -> Realizar Jogada")
    println("2 -> Undo") //Susceptivel a alterações, só faz sentido ter undo no caso do jogo já estar a rodar.
    println("3 -> Reiniciar")
    println("4 -> Sair")
  }

  @tailrec
  def askPieces(validPieces: List[Coord2D]): Coord2D = {

    println("De acordo com as Peças a seguir:")

    validPieces.foreach((x, y) => println(s"Position: ($x , $y)"))

    println("Escolha a linha da peça a jogar: ")
    val indL = getUserInputInt
    println("Escolha a coluna da peça a jogar: ")
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

    val result = play(state._1, state._2 , coordFroomPiece , coordToPiece , state._3)

    //(newBoard , switchPlayer(state._2) , newListOpnes)
    result match
      case (Some(newBoard), newOpenCoords) =>
        (newBoard, switchPlayer(state._2), newOpenCoords) //Aqui já devolvemos o novo estado como o currentPlayer( vamos já switch para o proximo jogador)
      case (None, _) =>
        println("Jogada inválida!")
        state //Aqui se calhar poderiamos repetir a jogada...

  }


  def ComputerMove(state: GameState, difficulty: Difficulty, rand: MyRandom): (GameState , MyRandom)= {

    val PositionCanPlay = listPlayablePositions(state._1, state._2, state._3) //Posições/Peças que podem ser usadas para jogar.
    val (coordFrom, r2) = randomMove(PositionCanPlay, rand) //Escolha de uma peça aleatoria

    val possibleDestinations = listValidDestinations(state._1, state._2, state._3) //Peças que podemos ir para lá jogar(Destinos válidos)

    val validDestinations = possibleDestinations.filter(coordTo => isValidPlay(state._1, state._2, coordFrom, coordTo, state._3))//De acordo com a Peça escolhida anterioramente de forma aleatoria vemos para onde esta pode ir.

    val (coordTo, r3) = randomMove(validDestinations, r2) //Escolha de destino aleatorio.

    val (newBoardOpt, newOpenCoords) = play(state._1, state._2, coordFrom, coordTo, state._3) //realizar primeira jogada.
    
    println(s"Jogada Realizada $coordFrom -> $coordTo")

    newBoardOpt match
      case None => (state,r3) //Devolvemos o estado Atual do jogo, Ou seja não houve jogada

      case Some(newBoard) =>
        val capturesLeft = difficultyLimit(difficulty) //O maximo de Capturas que podemos fazer (ex: Para cada dificuldade iremos ter um maximo de jogadas possiveis (hard não tem)

        val (finalBoard, finalOpenCoords, ryp) = continueCaptures(newBoard, state._2, coordTo, newOpenCoords, capturesLeft, r3) //Aqui fazemos multiplcas capturas
        val newstate = (finalBoard, switchPlayer(state._2), finalOpenCoords)

        (newstate, ryp)
        //Aqui já devolvemos a final board depois de fazer as multiplas capturas.
  }

  @tailrec
  def continueCaptures(board: Board, player: Stone, currentPos: Coord2D, openCoords: List[Coord2D], remainingCaptures: Int, rand: MyRandom): (Board, List[Coord2D], MyRandom) = {

    // Se não podemos continuar capturas , remaining Captures será o "capturesLeft" presente ComputerMove
    if remainingCaptures <= 0 then
      (board, openCoords, rand)
    else
      // Todas as posições vazias possíveis -> Para onde podemos jogar (o mesmo que foi feito no ComputerMove)
      val possibleDestinations = listValidDestinations(board, player, openCoords)

      val validDestinations = possibleDestinations.filter(coordTo => isValidPlay(board, player, currentPos, coordTo, openCoords))  // Filtramos apenas destinos válidos para a peça atual

      // Se não houver mais capturas possíveis
      if validDestinations.isEmpty then
        (board, openCoords, rand) //Devolvemos basicamente o que tinha chegado cá
      else

        val (nextCoordTo, r2) = randomMove(validDestinations, rand)// Escolhe próximo destino aleatoriamente
        // Executa mais um salto
        val (newBoardOpt, newOpenCoords) = play(board, player, currentPos, nextCoordTo, openCoords) // Executa mais um salto
        println(s"Jogada Realizada na captura multipla: $currentPos -> $nextCoordTo ")
        newBoardOpt match
          case None => (board, openCoords, r2) //Possivel erro (jogada não realizada , devolvemos oq já tinhamos
          case Some(newBoard) =>
            continueCaptures(newBoard, player, nextCoordTo, newOpenCoords, remainingCaptures - 1, r2) // Continua recursivamente
  }


  //Aqui que o jogo vai "rodar"
  @tailrec
  def gameLoop(state: GameState, history: GameHistory, timerLimit: Long, mode: GameMode, r: => MyRandom , diff: => Difficulty ): Unit = {

    println("Tabuleiro de Jogo: ")
    printBoard(state._1) //Print da Board
    println("")

    //Aqui basicamente vamos ver se alguem já ganhou. De acordo com o stone Atual.
    if isGameOver(state._1,state._2,state._3) then
      val winner = switchPlayer(state._2)
      println("Game Over!!!!!!!!!")
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
                val (result, newR) = ComputerMove(state , diff, r)
                (result,newR)

            case GameMode.CvC =>
              println("Computer vs Computer")
              val (result, newR) = ComputerMove(state , diff, r) //Aqui é sempre Computer Move.

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


  }


