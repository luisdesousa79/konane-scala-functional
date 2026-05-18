import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, TextField}
import javafx.scene.layout.{AnchorPane, BorderPane, GridPane}
import javafx.scene.control.ComboBox
import Difficulty.{Easy, Hard, Medium}
import GameMode.{CvC, PvC, PvP}
import javafx.scene.Scene
import javafx.stage.Stage


class MainMenuController {


  @FXML
  private var modoCombo: ComboBox[String] = _

  @FXML
  private var dificuldadeCombo: ComboBox[String] = _

  @FXML
  private var timerCombo: ComboBox[String] = _ //Depois tenho de inicializar os valores

  @FXML
  private var iniciarBtn: Button = _


  @FXML
  def initialize(): Unit = {

    modoCombo.getItems.addAll("Player vs Player", "Player vs Computador", "Computador vs Computador")
    dificuldadeCombo.getItems.addAll("Fácil", "Médio", "Difícil")
    timerCombo.getItems.addAll("1 minuto", "2 minutos", "3 minutos") //Passar para milissegundos em teoria depois

    //Valores default
    modoCombo.setValue("Player vs Player")
    dificuldadeCombo.setValue("Fácil")
    timerCombo.setValue("1 minuto")


  }

  def iniciarJogo(): Unit = {
    //Pattern mathcing de acordo com os valores passados pelos widgets combo.
    val modo = modoCombo.getValue match
      case "Player vs Player" => PvP
      case "Player vs Computador" => PvC
      case "Computador vs Computador" => CvC


    val dificuldade = dificuldadeCombo.getValue match
      case "Fácil" => Easy
      case "Médio" => Medium
      case "Difícil" => Hard

    // Lê o valor selecionado e converte para milissegundos
    val timer = timerCombo.getValue match
      case "1 minuto" => 60000L // 1 * 60 * 1000 ms
      case "2 minutos" => 120000L
      case "3 minutos" => 360000L


    abrirJanelaJogo(modo, dificuldade , timer)

  }

  private def abrirJanelaJogo(mode: GameMode, difficulty: Difficulty, timerLimit: Long): Unit = {

    val loader = new FXMLLoader(getClass.getResource("KonaneGame.fxml"))

    val root = loader.load[BorderPane]()

    val controller = loader.getController[KonaneGameController]

    controller.initializeGame(mode, difficulty, timerLimit)

    val stage = new Stage()
    stage.setTitle("Konane - Jogo") // Título da janela
    stage.setScene(new Scene(root)) // Adiciona os elementos à janela
    stage.show() // Mostra a janela

    iniciarBtn.getScene.getWindow.hide() //fechar a janela de conf

  }



}

