import Konane.*
import TUI.*

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

  def SetupDefGame(): Unit = {
    println("Configuarações de jogo ")

    //Vamos escolher aqui o modo de jogo
    val mode_game = choceGame

    //Escolha de Timer
    val timer = choiceTimer

    //Escolha de Tamanho
    val (lagura,comprimento) = choceTamanhoTab

    //Escolher Peças a serem removidas (as duas primeiras)

    //InitBoardAqui

    //Criar o GameSatete

    //Fazer return do Gamestate , Timer e GameMode


  }
  //Vamos fazer setup das condições de jogo sendo estas:  Tipo Jogo, Tamanho Tabuleiro , Peças a remover , Tempo Máximo de jogo(timer)


  def switchPlayer(player: Stone): Stone =
    player match
      case Stone.Black => Stone.White
      case Stone.White => Stone.Black

}
