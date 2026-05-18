import Konane.{Board, GameState, Stone}
import Konane.Stone.{Black, White}
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.{GridPane, StackPane, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.scene.shape.Rectangle
import javafx.scene.effect.DropShadow
import Konane.*
import Phase.{Capturing, InitialRemoval, Playing, SecondRemoval}
import javafx.scene.Scene
import javafx.fxml.FXMLLoader
import javafx.scene.Scene
import javafx.stage.Stage
import javafx.event.ActionEvent
import javafx.fxml.FXML
import GameMode.{CvC, PvC, PvP}
import Konane.*

import scala.annotation.tailrec
import scala.sys.exit

enum Phase:
  case InitialRemoval
  case SecondRemoval
  case Playing
  case Capturing

//Abaixo um case class
case class GameContext(state: GameState, phase: Phase, selected: Option[Coord2D], mode: GameMode, difficulty: Difficulty, timerLimit: Long, random: MyRandom, history: GameHistory)

class KonaneGameController {


  var currentContext: Option[GameContext] = None //Contexto de Jogo -> Podia melhorar? Mencionar que não faz ssentido ser Option -> Mudar


  @FXML
    private var currentPlayerLabel: Label =_
  @FXML
  private var gameModeLabel: Label = _
  @FXML
  private var timerLabel: Label = _
  @FXML var boardGrid: GridPane = _

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

    InfLayer.setText("Teste")
    val rand0 = MyRandom(10) //Random -> Apenas usado no jogo Player vs Computer ou Computer vs Computer

    computerButton.setVisible(mode == CvC)
    val tamanho = 6 //De acordo com o enunciado será este o Tamanho Fixo
    val initBoard = Konane.initBoard(tamanho,tamanho) //Inicializamos a Board de acordo com o tamanho fixo
    val gameState = (initBoard, Stone.Black ,List.empty ) //Estado de jogo Inicial (Board, Stone,LstOpenCoords)
    val gameContext = GameContext(gameState, Phase.InitialRemoval, None, mode,difficulty,timerLimit, rand0 , List()) //Contexto do jogo Inicial
    currentContext = Option(gameContext)
    drawnBoard(gameState)
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
  } //statico -> Nao mudamos

  //Função que cria uma celula com Peça
  def createCellula(x: Int, y: Int, stone: Stone): StackPane = {

    val pane = new StackPane() //Aqui que iremos colocar o "retangulo" e a peça correspondente (um circulo)
    pane.setPrefSize(70, 70) //Tamanho da StackPane.

    // Retangulo aonde será colocado a peça.
    val bg = new Rectangle(70, 70)
    // Aqui Basicamente estamos a colocar o fundo a alguma cor. (Bege Claro / Bege escurp)
    if ((x + y) % 2 == 0)
      bg.setFill(Color.web("#E8D5B7")) // bege claro
    else
      bg.setFill(Color.web("#D2B48C")) // bege escuro


    //Colocar "arredondamento nos cantos o retangulo"
    bg.setArcWidth(12)
    bg.setArcHeight(12)

    bg.setStroke(Color.web("#4A3B2A")) // Cor da Borda
    bg.setStrokeWidth(2) //Espessura da Borda

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
    pane.setOnMouseEntered(_ => {pane.setScaleX(1.05)
      pane.setScaleY(1.05)})

    pane.setOnMouseExited(_ => {pane.setScaleX(1.0)
      pane.setScaleY(1.0)})

    pane.setOnMouseClicked(_ => HandleBoardClick(x,y))
    //Entered -> Quando mousse "entra"
    //Excited -> Quando o mousse "Sai"

    pane //peça a devolver
  } //statico -> Não mudamos


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
  } //statico -> Não mudamos


  //Funçao que irá tratar o jogo Humano, de acordo com o contexto escolhe uma função de jogada a realizar
  @FXML
  def HandlePlayerMove(x: Int, y:Int): Unit = {
    println(s"A peça selecionada é $x e $y")
    currentContext match
      case Some(contexto) =>
        contexto.phase match
          case Phase.InitialRemoval =>  chooseInitPiecesToRemove(x,y)
          case Phase.Playing => NormalGame(x,y)
          case Phase.Capturing => MultiplaCaptura(x,y)
          case SecondRemoval => chooseSecondPieceToRemove(x,y)
      case None => //Irmos embora secalhar -> Mudar Isto
  }

  //Segunda Peça para escolher -> Função de Player (entramos aqui quando for preciso entrar na segunda remoção
  def chooseSecondPieceToRemove(x: Int , y: Int): Unit = {
    val coordSelected = (x,y) //coordenada que chega
    currentContext match
      case Some(contexto) => //caso tenhamos algum contexto de jogo
        val firstRemoved = contexto.state._3.head //pegamos a primeira peça a remover -> Só existe esta na lista de peças abertas
        val isAdj = Konane.isAdj(coordSelected,firstRemoved) //vemos se a peça que temos é adjacente, no caso de não ser -> não fazemos nada e não mudamos de estado de jogo
        if(isAdj) then
          val newBoard = Konane.removePecas(contexto.state._1,List(coordSelected))  //removemos a peça
          val newOpenLst = contexto.state._3 :+ coordSelected //adicionamos a peça vazia ao tabuleiro
          val newGameState = new GameState(newBoard,switchPlayer(contexto.state._2),newOpenLst) // novo GameState  -> Fazemos troca do currentPlayer(switchPlayer)
          currentContext = Some(contexto.copy(state = newGameState, phase = Playing)) //Mudamos o GameState e a fase de jogo
          drawnBoard(newGameState) //desenhamos.
      case None =>
  }
  //Função de Modo Player vs Player
  def chooseInitPiecesToRemove(x: Int,y: Int): Unit = {
    println("chooseInitPiecesToRemove")
    currentContext match
      case Some(contexto) =>
        val coordenadas = (x,y) //Coordenada Selecionada //A primeira sempre a jogar é a peça Preta (Hardcoded) -> Damos as peças Iniciais a remover.
        val RemovePiecesCan = GameEngine.pecasRemover2(6,6) //Lista de Peças Possiveis a remover na primeira interação( os cantos, de acordo com o tamanho do tabuleiro)
        RemovePiecesCan.contains(coordenadas) match //match de modo a ver se a peça selecionada condiz com as peças possiveis a remover.
          case true =>
            val newBoard= Konane.removePecas(contexto.state._1 , List(coordenadas)) //removemos as peças -> Obtemos novaBoard
            val newOpenList = contexto.state._3 :+ coordenadas //Adicionamos a nova peça às Peças Abertas
            val newState = (newBoard, switchPlayer(contexto.state._2), newOpenList) //novo GameSatate , troca de player Atual
            currentContext = Some(contexto.copy(state = newState, phase = SecondRemoval)) //mudamos a fase de jogo para SecondRemoval e atualizamos o state
            drawnBoard(newState) //desenhamos a nova bord
          case _ => //não fazemos nada -> Mantemos o mesmo estado
      case None => //Nunca chegamos neste caso
  } //Melhorar aqui, de acordo com a peça temos logo que tirar a proxima.

  def MultiplaCaptura(x: Int,y: Int): Unit = {
    val coord = (x,y) //Coordenada que chegou Aqui.
    currentContext match
      case Some(contexto) => //Caso tenhamos algum contexto:
        contexto.selected match
          case Some(selected) => //Caso tenhamos alguma peça selecionada
            if(coord == selected) then
              val state1 = contexto.state.copy(_2 = switchPlayer(contexto.state._2)) //Cpiamos o contexto e mudamos para o proximo jogador
              currentContext = Some(contexto.copy(phase = Playing , state = state1 , selected = None)) //colocamos o novo contexto de jogo, como em Playing
              drawnBoard(state1) //Desenhamos a board de acordo com o novo estado (que vai incluir a board) -> State1
            else
              val canplay = Konane.isValidPlay(contexto.state._1,contexto.state._2,selected,coord,contexto.state._3) //No caso de ser possivel jogar
              canplay match
                case true =>
                  val (newBoard, newLstOpenCoords) = Konane.play(contexto.state._1,contexto.state._2,selected,coord,contexto.state._3) //Realizamos uma jogada
                  newBoard match
                    case Some(newTab) =>
                      //Ver se podemos ou não continuar a captura
                      if(listPlayablePositions(newTab , contexto.state._2 , newLstOpenCoords).contains(coord)) then
                        println("Bora Podes Continuar a jogar")
                        val newState = new GameState(newTab, contexto.state._2, newLstOpenCoords) //novo estado
                        currentContext = Some(contexto.copy(state = newState , selected = Some(coord))) //novo contexto
                        drawnBoard(newState)//desenhamos a board
                      else //caso não dê para continuar a jogar
                        val newState2 = new GameState(newTab, switchPlayer(contexto.state._2), newLstOpenCoords) //novo GameState
                        val RecentHistory = contexto.state :: contexto.history  //Nova historia
                        currentContext = Some(contexto.copy(state = newState2 , selected = None , phase = Playing , history = RecentHistory)) //novo contexto, mudamos para playing novamente
                        drawnBoard(newState2) //Realizamos uma jogada e já desenhamos na tela.
                    case None =>
                case _ => println("Nao pode jogar")
          case None =>
      case None =>

  }

  def NormalGame(x: Int,y: Int): Unit = { //A pessoa seleciona uma Peça (O jogo já iniciou)
    currentContext match //De acordo com o contexto (Ao ter um contexto vamos ver o tipo de jogo a realizar)
      case Some(contexto) =>
        val coord = (x, y) //cordenadas Selecionadas
        val StoneSelected = contexto.state._1.get(x,y) //Peça selecionada no local
        //println(s" A peça Selecionada é $StoneSelected")
        contexto.selected match //Vamos ver se já temos algo selecionada ou não
          case None => //Se não tivermos temos que ver se é valida ou não a jogar.

            val PosicoesJogaveis = listPlayablePositions(contexto.state._1 , contexto.state._2 , contexto.state._3)
            PosicoesJogaveis.contains(coord) match
              case true => currentContext = Some(contexto.copy(selected = Some(coord))) //Colocamos a peça jogavel como selecionada
              //Aqui depois podemos dar um contexto melhor, ou seja colocar as peças com maior destaque(para onde ele pode jogar)
              case _ =>

          case Some(pecaOrigem) => //Aqui vamos ver se a podemos realizar a jogada.
            Konane.isValidPlay(contexto.state._1, contexto.state._2, pecaOrigem, coord, contexto.state._3) match
              case true =>
                val (newBoard, newLstOpenCoords) = Konane.play(contexto.state._1,contexto.state._2,pecaOrigem,coord,contexto.state._3) //Realizamos uma jogada
                //val newState = (newBoard,GameEngine.switchPlayer(contexto.state._2), newLstOpenCoords)
                newBoard match
                  case Some(newTab) =>
                    //Ver se podemos ou não continuar a captura
                    if(listPlayablePositions(newTab , contexto.state._2 , newLstOpenCoords).contains(coord)) then
                      val newState = new GameState(newTab, contexto.state._2, newLstOpenCoords)
                      currentContext = Some(contexto.copy(state = newState , phase = Capturing , selected = Option(coord)))
                      print(s" A lista de posições livres é: $newLstOpenCoords")
                      print(s"A peça Selecionada é: $pecaOrigem")
                      drawnBoard(newState)
                    else
                      val RecentHistory = contexto.state :: contexto.history  //Nova historia
                      val newState2 = new GameState(newTab, switchPlayer(contexto.state._2), newLstOpenCoords)
                      currentContext = Some(contexto.copy(state = newState2 , selected = None , history = RecentHistory))
                      drawnBoard(newState2) //Realizamos uma jogada e já desenhamos na tela.
                  case None =>
              case _=> currentContext = Some(contexto.copy(selected = Some(coord))) //Apenas colocamos como peça selecionada

      case None =>

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
  def ComputerJoga(): Unit = {
    println("Computador vai jogar")
    currentContext match
      case Some(contexto) =>
        contexto.phase match
          case InitialRemoval=>
            val listasPecasRemove = GameEngine.pecasRemover2(6,6)
            val (pecaRemover , newRandom ) = Konane.randomMove(listasPecasRemove , contexto.random)
            val newBoard = Konane.removePecas(contexto.state._1, List(pecaRemover))
            val newOpenLst = contexto.state._3 :+ pecaRemover
            val newState = new GameState(newBoard,switchPlayer(contexto.state._2), newOpenLst)
            currentContext = Some(contexto.copy(state = newState , phase = SecondRemoval , random = newRandom))
            drawnBoard(newState)
          case SecondRemoval =>
            println("SecondRemoval do PC")
            val firstRemoved = contexto.state._3.head
            val SecondPieceRemove = Konane.AdjPieces(firstRemoved)
            val (secondPieceRemoveChosse , newRandom) = Konane.randomMove(SecondPieceRemove, contexto.random)
            val newBoard = Konane.removePecas(contexto.state._1, List(secondPieceRemoveChosse))
            val newOpenList = contexto.state._3 :+ secondPieceRemoveChosse
            val newState = new GameState(newBoard,switchPlayer(contexto.state._2), newOpenList)
            currentContext = Some(contexto.copy(state = newState , random = newRandom , phase = Playing))
            drawnBoard(newState)
          case  Playing =>
            //ComputerMove(state: GameState, difficulty: Difficulty, rand: MyRandom): (GameState , MyRandom)
            val RecentHistoy = contexto.state :: contexto.history  //novo gameState na history
            val (newState , newR , _ ) = ComputerMove(contexto.state,contexto.difficulty , contexto.random)
            currentContext = Some(contexto.copy(state = newState , random = newR , history = RecentHistoy))
            drawnBoard(newState)
          case Capturing => //Nunca chega nesta fase
      case _ => //Print nunca chegamos neste caso


  }

  @FXML
  def UndoGame(): Unit = {
    println("A Fazer Undo")
    Konane.undoMove(currentContext.get.history) match
      case Some((newState, newHistory)) =>
        currentContext match
          case Some(contexto) =>
            currentContext = Some(contexto.copy(state = newState , history = newHistory))
            drawnBoard(newState)
          case None =>
      case None => println("Nao possivel fazer Undo")

  }

  @FXML
  def LeaveGame(): Unit = {
    println("A sair do jogo")
    exit()
  }

  def HandleBoardClick(x: Int, y: Int): Unit =
    currentContext match
      case Some(ctx) =>
        ctx.mode match
          case GameMode.CvC =>
            InfLayer.setText("Use o botão 'Realizar Jogada'")
          case GameMode.PvP =>
            HandlePlayerMove(x, y)
          case GameMode.PvC =>
            ctx.state._2 match
              case Stone.Black =>
                // humano
                HandlePlayerMove(x, y)
                checkComputerTurn()
              case Stone.White =>
                // computador
                InfLayer.setText("A aguardar jogada do computador")
      case None =>
    currentContext match
      case Some(ctx) =>
        val finishGame = isGameOver(ctx.state._1, ctx.state._2, ctx.state._3)
        val fase = ctx.phase == Phase.InitialRemoval || ctx.phase == Phase.SecondRemoval
        if finishGame && !fase then
          InfLayer.setText(s"Jogou Acabou ${switchPlayer(ctx.state._2)} Ganhou! ")
          //Tratar do Fim de jogo
      case None =>


  def checkComputerTurn(): Unit =
    currentContext match
      case Some(ctx) =>
        ctx.mode match

          case GameMode.PvC =>
            // White = computador
            if (ctx.state._2 == Stone.White) then
              ComputerJoga()
          case _ =>
      case None =>
}
