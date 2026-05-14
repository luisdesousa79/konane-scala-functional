import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.GridPane
import Konane.Stone

class Controller {

  @FXML var boardGrid: GridPane = _
  @FXML var infoLabel: Label = _

  private val tamanho: Int = 5
  private val botoes: Array[Array[Button]] = Array.ofDim[Button](tamanho, tamanho)

  private var board: Konane.Board = _
  private var lstOpenCoords: List[(Int, Int)] = _
  private var currentPlayer: Stone = Stone.Black

  @FXML
  def initialize(): Unit = {
    // Cria os botões do tabuleiro
    for (i <- 0 until tamanho) {
      for (j <- 0 until tamanho) {
        val btn = new Button(".")
        btn.setMinSize(60, 60)
        btn.setStyle("-fx-font-size: 20px; -fx-font-weight: bold;")
        btn.setOnAction(_ => onCellClicked(i, j))
        botoes(i)(j) = btn
        boardGrid.add(btn, j, i)
      }
    }
    iniciarJogo()
  }

  private def iniciarJogo(): Unit = {
    val removida = (tamanho / 2, tamanho / 2)
    board = Konane.initBoard(tamanho, List(removida))
    lstOpenCoords = calcularPosicoesVazias()
    currentPlayer = Stone.Black
    atualizarTabuleiro()
    infoLabel.setText("Jogo iniciado! Jogador: PRETO (B)")
  }

  def onCellClicked(row: Int, col: Int): Unit = {
    // Por agora, só mostra o que tem na posição
    board.get((row, col)) match {
      case Some(Stone.Black) =>
        infoLabel.setText(s"Posicao ($row, $col) - Peca PRETA (B)")
      case Some(Stone.White) =>
        infoLabel.setText(s"Posicao ($row, $col) - Peca BRANCA (W)")
      case None =>
        infoLabel.setText(s"Posicao ($row, $col) - VAZIO (.)")
    }
  }

  private def atualizarTabuleiro(): Unit = {
    for (i <- 0 until tamanho) {
      for (j <- 0 until tamanho) {
        val coord = (i, j)
        botoes(i)(j).setText(
          board.get(coord) match {
            case Some(Stone.Black) => "B"
            case Some(Stone.White) => "W"
            case None => "."
          }
        )
      }
    }
  }

  private def calcularPosicoesVazias(): List[(Int, Int)] = {
    (for {
      i <- 0 until tamanho
      j <- 0 until tamanho
      if !board.contains((i, j))
    } yield (i, j)).toList
  }
}