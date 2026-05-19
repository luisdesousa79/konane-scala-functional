import Konane.{Board, GameState, Stone}
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.{GridPane, StackPane, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.scene.shape.Rectangle
import javafx.scene.effect.DropShadow
import javafx.fxml.FXMLLoader
import javafx.scene.Scene
import javafx.stage.Stage
import javafx.fxml.FXML
import GameMode.*
import Konane.*
import scala.annotation.tailrec
import scala.sys.exit



class KonaneGameController {

  var currentContext: Option[GameContext] = None //Contexto de Jogo 

  // Peça atualmente selecionada
  var selectedCell: Option[(Int, Int)] = None

  // Lista de jogadas possíveis
  var possibleMoves: List[(Int, Int)] = List()

  @FXML
  private var currentPlayerLabel: Label =_

  @FXML
  private var gameModeLabel: Label = _

  @FXML
  private var timerLabel: Label = _

  @FXML
  var boardGrid: GridPane = _

  @FXML
  private var InfLayer: Label = _

  @FXML
  private var Undo: Button = _

  @FXML
  private var Sair: Button = _

  @FXML
  private var computerButton: Button = _

  @FXML
  private var Restart: Button =_

  def initializeGame(mode: GameMode, difficulty: Difficulty, timerLimit: Long): Unit = {

    val rand0 = MyRandom(10) //Random -> Apenas usado no jogo Player vs Computer ou Computer vs Computer

    computerButton.setVisible(mode == CvC || mode == PvC)

    val initBoard = Konane.initBoard(6,6) //Inicializamos a Board de acordo com o tamanho fixo

    val gameState = (initBoard, Stone.Black ,List.empty ) //Estado de jogo Inicial (Board, Stone,LstOpenCoords)

    val gameContext = GameContext(
      gameState,
      Phase.InitialRemoval,
      None,
      mode,
      difficulty,
      timerLimit,
      rand0,
      List(),
      6,
      6
    ) //Contexto do jogo Inicial

    currentContext = Option(gameContext)

    drawnBoard(gameState)

    updateInterfaceFeedback(gameContext)
  }

  def drawnBoard(gameState: GameState): Unit = {

    boardGrid.getChildren.clear()

    @tailrec
    def loop(cells: List[((Int, Int), Stone)]): Unit =
      cells match
        case Nil =>

        case ((x, y), stone) :: tail =>
          val cell = createCellula(x, y, stone)
          boardGrid.add(cell, y, x) //Ter em atenção que no GridPane (column, row)
          loop(tail)

    loop(gameState._1.toList)

    @tailrec
    def loop2(cells:  List[(Int, Int)] ): Unit = {
      cells match

        case Nil =>

        case (x,y) :: tail =>
          val cell = createEmptyCell(x,y)
          boardGrid.add(cell,y,x)
          loop2(tail)
    }

    loop2(gameState._3)

    boardGrid.requestLayout()
  }

  //Função que cria uma celula com Peça
  def createCellula(x: Int, y: Int, stone: Stone): StackPane = {

    val pane = new StackPane() //Aqui que iremos colocar o "retangulo" e a peça correspondente (um circulo)

    pane.setPrefSize(70, 70) //Tamanho da StackPane.

    // Retangulo aonde será colocado a peça.
    val bg = new Rectangle(70, 70)

    // Aqui Basicamente estamos a colocar o fundo a alguma cor. (Bege Claro / Bege escuro)
    if ((x + y) % 2 == 0)
      bg.setFill(Color.web("#E8D5B7")) // bege claro
    else
      bg.setFill(Color.web("#D2B48C")) // bege escuro


    //Colocar "arredondamento nos cantos o retangulo"
    bg.setArcWidth(12)
    bg.setArcHeight(12)

    bg.setStroke(Color.web("#4A3B2A")) // Cor da Borda
    bg.setStrokeWidth(2) //Espessura da Borda

    // Highlight peça selecionada
    selectedCell match
      case Some((sx, sy)) if sx == x && sy == y =>
        bg.setStroke(Color.GOLD)
        bg.setStrokeWidth(5)

      case _ =>

    // Aqui que iremos colocar a peça (será um circulo)
    val piece = new Circle(24)

    stone match

      case Stone.Black =>

        piece.setFill(Color.web("#1E1E1E")) //cor da peça
        piece.setStroke(Color.web("#555555")) // Cor da borda (contorno)
        piece.setStrokeWidth(2) //espessura da borda (como já foi abordado lá em cima)

        // sombra
        piece.setEffect(new DropShadow(10, Color.rgb(0, 0, 0, 0.45))) //10 -> raio da sombra , rgb normal , o valor 0,45 é a opacidade

      case Stone.White =>

        piece.setFill(Color.web("#F8F8F8"))
        piece.setStroke(Color.web("#BBBBBB"))
        piece.setStrokeWidth(2)
        piece.setEffect(new DropShadow(10, Color.rgb(0, 0, 0, 0.45)))

    pane.getChildren.addAll(bg, piece) //adicionamentos os elementos (retangulo) e (circulo) ao Stack Pane(elemento que irá estar em cada posição do GridPane)

    //Acima funciona como uma especie de impelhação, sendo assim o elemento piece irá ficar um layer acima , retangulo representa o fundo.

    // animação hover (ao passar o mousse por cima e a retirar)
    pane.setOnMouseEntered(_ => {
      pane.setScaleX(1.05)
      pane.setScaleY(1.05)
    })

    pane.setOnMouseExited(_ => {
      pane.setScaleX(1.0)
      pane.setScaleY(1.0)
    })

    pane.setOnMouseClicked(_ => HandleBoardClick(x,y))

    //Entered -> Quando mousse "entra"
    //Excited -> Quando o mousse "Sai"

    pane //peça a devolver
  }

  //Função que Cria uma celula Vazia (as peças presentes em ListopenCords do GameState
  def createEmptyCell(x: Int, y: Int): StackPane = {

    val pane = new StackPane() //Widget que será colocado na Grid

    pane.setPrefSize(70, 70) //Tamanho

    val bg = new Rectangle(70, 70) //Fundo (Apenas será colocado este widget no StackPane)

    (x + y) % 2 match
      case 0 => bg.setFill(Color.web("#E8D5B7"))
      case _ => bg.setFill(Color.web("#D2B48C"))

    //Mesma Logica que na função acima
    bg.setArcWidth(12)
    bg.setArcHeight(12)

    bg.setStroke(Color.web("#4A3B2A"))
    bg.setStrokeWidth(2)

    // Mostrar movimentos válidos
    if (possibleMoves.contains((x, y))) {

      bg.setStroke(Color.LIMEGREEN)
      bg.setStrokeWidth(5)

      //Glow visual
      bg.setEffect(new DropShadow(15, Color.LIMEGREEN))
    }

    pane.getChildren.add(bg) //Adicionamos o retangulo(espaço vazio)

    //Mesma Logica que na função acima

    //Mouse entrar
    pane.setOnMouseEntered(_ =>
      pane.setScaleX(1.05)
      pane.setScaleY(1.05)
    )

    //Mousse sair
    pane.setOnMouseExited(_ =>
      pane.setScaleX(1.0)
      pane.setScaleY(1.0)
    )

    pane.setOnMouseClicked(_ => HandleBoardClick(x,y))

    pane
  }

  @FXML
  def RestartGame(): Unit = {

    // Carregar o FXML
    val fxmlLoader = new FXMLLoader(getClass.getResource("MainMenu.fxml"))

    val root = fxmlLoader.load[VBox]()

    // Criar nova janela
    val stage = new Stage()

    stage.setTitle("Menu")
    stage.setScene(new Scene(root))
    stage.show()

    // Fechar janela atual
    Restart.getScene.getWindow.asInstanceOf[Stage].close()
  }

  @FXML
  def ComputerJoga() : Unit = {

    currentContext match

      case Some(contexto) =>

        contexto.mode match

          case GameMode.PvC =>

            // White = computador
            if (contexto.state._2 == Stone.White) {

              val resultado = JogoCumpter(contexto)

              currentContext = Some(resultado.context)

              selectedCell = None
              possibleMoves = List()

              drawnBoard(resultado.context.state)

              updateInterfaceFeedback(resultado.context)
            }
            else {
              InfLayer.setText("Ainda é a vez do jogador humano.")
            }

          case GameMode.CvC =>

            val resultado = JogoCumpter(contexto)

            currentContext = Some(resultado.context)

            selectedCell = None
            possibleMoves = List()

            drawnBoard(resultado.context.state)

            updateInterfaceFeedback(resultado.context)

          case _ =>

            InfLayer.setText("Não existe computador neste modo.")

      case None =>
  }

  @FXML
  def UndoGame(): Unit = {

    Konane.undoMove(currentContext.get.history) match

      case Some((newState, newHistory)) =>

        currentContext match

          case Some(contexto) =>

            currentContext = Some(
              contexto.copy(
                state = newState,
                history = newHistory
              )
            )

            selectedCell = None
            possibleMoves = List()

            drawnBoard(newState)

          case None =>

      case None => // Possivelmente colocae alguma mensagem aqui.
  }

  @FXML
  def LeaveGame(): Unit = {
    exit()
  }

  def HandleBoardClick(x: Int, y: Int): Unit = {
    currentContext match {
      case Some(ctx) =>

        // Bloqueio se for turno do computador no modo PvC
        if (ctx.mode == GameMode.PvC && ctx.state._2 == Stone.White) {

          InfLayer.setText("Clique no botão do computador para jogar.")

        } else if (ctx.mode == GameMode.CvC) {

          //Aqui basicamente apenas ao clicar no botão Realizar Jogada -> De modo a ser Melhor!
          InfLayer.setText("Use o botão 'Realizar Jogada'. No modo CvC o computador joga sozinho.")

        } else {

          // Aqui estamos Basicamente num jogo em que o Player há de jogar(humano)
          Konane.processInteraction(ctx, (x, y)) match {

            //Aqui vamos processar a interação.
            case Some(newCtx) =>
              // Atualizar seleção atual
              selectedCell = newCtx.selected
              possibleMoves =
                newCtx.selected match
                  case Some(pos) =>
                    getValidInteractions(newCtx.state,newCtx.phase,selectedCell,newCtx.rows,newCtx.columns)

                  case None =>
                    List()

              currentContext = Some(newCtx) //Novo Current context

              drawnBoard(newCtx.state) //desenho do estado Atual.

              updateInterfaceFeedback(newCtx) // Fornecer feedback visual ao utilizado

            case None =>

              selectedCell = None
              possibleMoves = List()

              InfLayer.setText("Jogada ou seleção inválida!")
          }
        }

      case None =>
    }
  }

  // Dar mais contexto Aqui.
  // Ter mais labels, um label para Turno de jogador.
  // um label para avisos , um label a indicar oq fazer.
  def updateInterfaceFeedback(ctx: GameContext): Unit = {

    ctx.phase match {

      case Phase.InitialRemoval =>

        InfLayer.setText("Remova a primeira peça (Preta).")

      case Phase.SecondRemoval =>

        InfLayer.setText("Remova uma peça Branca adjacente.")

      case Phase.Capturing =>

        InfLayer.setText("Pode continuar a capturar ou clique na sua peça para terminar.")

      case Phase.Playing =>

        if (Konane.isGameOver(ctx.state._1, ctx.state._2, ctx.state._3)) {

          InfLayer.setText(s"Fim de Jogo! Vencedor: ${switchPlayer(ctx.state._2)}")

          //Logica de Acabar o jogo Aqui.

        } else {

          InfLayer.setText(s"Turno do jogador: ${ctx.state._2}")
        }
    }
  }
}