import Konane.*
import TUI.*
import  Konane.GameState

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import scala.util.Try

class GameEngine {

  enum GameMode:
    case PvP, PvC , CvC



  def showMenu(): Unit = {
    print("1. Iniciar Jogo \n")
    print("2. Sair \n")

    getUserInputInt match
      case 1 => print("SetupGame()")
      case 2 => print("A sair.")
      case _ => {print("Opcão Inválida!")
      showMenu()}
  }

  def getUserInputInt: Int = {
    readInt()
  }

  def choceGame: GameMode = {
    println("Modo de jogo:")
    println("1 - Player vs Player")
    println("2 - Player vs Computador")
    println("3 - Computador vs Computador")
    getUserInputInt match
      case 1 => GameMode.PvP
      case 2 => GameMode.PvC
      case 3 => GameMode.CvC
      case _ => println("Escolha uma opcão válida!")
      choceGame
  }


  def choiceTimer: Long = {

    println("Escolha o timer (mínimo 1 minuto):")

    val result = Try(getUserInputInt.toLong).toOption

    result match {
      case Some(time) if time >= 1 => time * 60 * 1000 //Estamos a converter para milissegundos

      case _ =>
        println("Valor inválido.")
        choiceTimer
    }
  }

  def choceTamanhoTab: (Int,Int) = {

    println("Vamos escolher o tamanho do Tabuleiro!")
    println("Largura:")
    val largura = getUserInputInt

    //Aplicar restriçoes de Tamanho Aqui.

    print("Comprimento:")
    val comprimento = getUserInputInt
    (largura,comprimento)
  }

  def possibleInitialRemovals(rows: Int, cols: Int): List[(Coord2D, Coord2D)] = {
    List(((0, cols - 2), (0, cols - 1)), ((rows / 2, cols / 2 - 1), (rows / 2, cols / 2)), ((rows - 1, 0), (rows - 1, 1)))
    //Devolvemos uma lista de conjunto de posições livres. Tendo em conta que as regras konane apenas permitem remover no meio e no canto superior direito , inferior esquerdo.
  }

  //Tail recursive Modificar
  def choceInitPosToRemove(rows: Int, cols: Int): List[Coord2D] = {
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
        choceInitPosToRemove(rows, cols)}
  }

  def SetupDefGame(): (GameState, Long, GameMode) = {
    println("Configuarações de jogo ")

    //Vamos escolher aqui o modo de jogo
    val mode_game = choceGame

    //Escolha de Timer
    val timer = choiceTimer

    //Escolha de Tamanho
    val (lagura,comprimento) = choceTamanhoTab

    //Escolher Peças a serem removidas (as duas primeiras)
    val removed = choceInitPosToRemove(lagura,comprimento)

    //InitBoardAqui + Remover as peças Inicias Aqui
    val board0 = removePecas(initBoard(lagura,comprimento), removed)

    //Criar o GameSatet
    val gameSate0 = new GameState(board0,Stone.Black,removed) //Podemos Melhorar , no caso de ser PvP ou PvC a pessoa poder escolher a sua peça (Preta ou Branca)

    //Fazer return do Gamestate , Timer e GameMode
    (gameSate0,timer,mode_game)


  }
  //Vamos fazer setup das condições de jogo sendo estas:  Tipo Jogo, Tamanho Tabuleiro , Peças a remover , Tempo Máximo de jogo(timer)

  def switchPlayer(player: Stone): Stone =
    player match
      case Stone.Black => Stone.White
      case Stone.White => Stone.Black

}
