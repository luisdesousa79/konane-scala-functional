import Difficulty.{Easy, Hard, Medium}
import Phase.{Capturing, InitialRemoval, Playing, SecondRemoval}
import GameEngine.{PlayerMove, difficultyLimit}
import com.sun.source.tree.WhileLoopTree

import scala.collection.parallel.immutable.ParMap
import scala.annotation.tailrec
import GameMode.*
object Konane:

  type Coord2D = (Int, Int)

  type Board = ParMap[Coord2D, Stone]

  enum Stone:
    case Black, White
    
  type GameState = (Board, Stone, List[Coord2D])
  
  type GameHistory = List[GameState]


  def FistPieceToRemove(row: Int, cols: Int): List[Coord2D] = { //Jogo GUI
    List( (0, 0), (row - 1, cols - 1))
  }

  def possibleInitialRemovals(rows: Int, cols: Int): List[(Coord2D, Coord2D)] = {
    List(((0, cols - 2), (0, cols - 1)), ((rows / 2, cols / 2 - 1), (rows / 2, cols / 2)), ((rows - 1, 0), (rows - 1, 1)))

    //Devolvemos uma lista de conjunto de posições livres. Tendo em conta que as regras konane apenas permitem remover no meio e no canto superior direito , inferior esquerdo.
  }

  def nextFase(fase: Phase): Phase = {
    fase match
      case InitialRemoval => SecondRemoval
      case SecondRemoval => Phase.Playing
      case Playing => Phase.Capturing
      case Capturing => Playing
  }


  def switchPlayer(player: Stone): Stone =
    player match
      case Stone.Black => Stone.White
      case Stone.White => Stone.Black


  def maxCaptures(diff: Difficulty): Int =
    diff match
      case Easy => 1
      case Medium => 2
      case Hard => Int.MaxValue


  // T1
  // função que implementa um movimento aleatório
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    // escolhe aleatoriamente um índice da lista de coordenadas vazias
    // atenção que isto não procura posições jogáveis (Adjacentes) - isso é feito pela função play
    val (randomIndex, newRand) = rand.nextInt(lstOpenCoords.length)

    // guarda o novo gerador de números aleatórios
    val newRandState = newRand.asInstanceOf[MyRandom]

    // guarda a coordenada que corresponde ao índice aleatório escolhido
    val coord = lstOpenCoords(randomIndex)

    //devolve coordenada e nova seed
    (coord, newRandState)
  }

  def removePecas(board: Board, removed: List[Coord2D]): Board =

    removed.foldLeft(board) {
      case (b, coord) => b - coord}

  def AdjPieces(coordenada: Coord2D): List[Coord2D] = {
    val (x, y) = coordenada
    List((x - 1, y), // esquerda
      (x + 1, y), // direita
      (x, y - 1), // cima
      (x, y + 1)  // baixo
    ).filter { case (nx, ny) =>
      nx >= 0 && nx <= 5 &&
        ny >= 0 && ny <= 5}
  }


  def ComputerMove(state: GameState, difficulty: Difficulty, rand: MyRandom): (GameState, MyRandom , List[(Coord2D,Coord2D)]) = {

    val PositionCanPlay = listPlayablePositions(state._1, state._2, state._3) //Posições/Peças que podem ser usadas para jogar.
    val (coordFrom, r2) = randomMove(PositionCanPlay, rand) //Escolha de uma peça aleatoria

    val DestinosValidos = validDestinationsFromPiece(state._1 , state._2, coordFrom,state._3) //Irá calcular de acordo com o estado de jogo as posições para onde vamos jogar (A peça que chega aqui é jogavel)
    val (coordTo, r3) = randomMove(DestinosValidos, r2) //Escolha de destino aleatorio.
    val (newBoardOpt, newOpenCoords) = play(state._1, state._2, coordFrom, coordTo, state._3) //realizar primeira jogada.

    val Jogada = List((coordFrom, coordTo)) //Guardamos a primeira jogada.

    newBoardOpt match
      case None => (state, r3 , List()) //Devolvemos o estado Atual do jogo, Ou seja não houve jogada
      case Some(newBoard) =>
        val capturesLeft = difficultyLimit(difficulty) //O maximo de Capturas que podemos fazer (ex: Para cada dificuldade iremos ter um maximo de jogadas possiveis (hard não tem)
        val (finalBoard, finalOpenCoords, ryp , jogadas) = continueCaptures(newBoard, state._2, coordTo, newOpenCoords, capturesLeft, r3 , Jogada) //Aqui fazemos multiplcas capturas
        val newstate = (finalBoard, switchPlayer(state._2), finalOpenCoords)
        (newstate, ryp , jogadas)
    //Aqui já devolvemos a final board depois de fazer as multiplas capturas.
  }
  @tailrec
  def continueCaptures(board: Board, player: Stone, currentPos: Coord2D, openCoords: List[Coord2D], remainingCaptures: Int, rand: MyRandom , jogadasFeitas: List[(Coord2D,Coord2D)] ): (Board, List[Coord2D], MyRandom , List[(Coord2D,Coord2D)]) = {
    // Se não podemos continuar capturas , remaining Captures será o "capturesLeft" presente ComputerMove
    if remainingCaptures <= 0 then
      (board, openCoords, rand , jogadasFeitas)
    else
      // Todas as posições vazias possíveis -> Para onde podemos jogar (o mesmo que foi feito no ComputerMove)

      val validDestinations = validDestinationsFromPiece(board,player,currentPos,openCoords) //no caso de termos posições válidas.
      // Se não houver mais capturas possíveis
      if validDestinations.isEmpty then
        (board, openCoords, rand , jogadasFeitas) //Devolvemos basicamente o que tinha chegado cá
      else
        val (nextCoordTo, r2) = randomMove(validDestinations, rand) // Escolhe próximo destino aleatoriamente
        // Executa mais um salto
        val (newBoardOpt, newOpenCoords) = play(board, player, currentPos, nextCoordTo, openCoords) // Executa mais um salto
        val novaJogada = jogadasFeitas :+ (currentPos,nextCoordTo)
        newBoardOpt match
          case None => (board, openCoords, r2,jogadasFeitas) //Possivel erro (jogada não realizada , devolvemos oq já tinhamos
          case Some(newBoard) =>
            continueCaptures(newBoard, player, nextCoordTo, newOpenCoords, remainingCaptures - 1, r2, novaJogada) // Continua recursivamente
  }



  def isAdj(coordenada: Coord2D, coordenadaRemovida: Coord2D): Boolean = {
    val (x1, y1) = coordenada
    val (x2, y2) = coordenadaRemovida
    (math.abs(x1 - x2) == 1 && y1 == y2) ||
    (math.abs(y1 - y2) == 1 && x1 == x2)}


  //T2 - Funcao Initboard e Play
  // função que inicializa o tabuleiro, de modo a podermos ter tabuleiros quadrados ou retangulares recebemos dois argumentos, a remoçao das peças é feita depois.
  def initBoard(rows: Int, cols: Int): Board = {

    @tailrec
    def loop(row: Int, col: Int, acc: Board): Board =
      (row, col) match {

        case (r, _) if r >= rows => acc //Caso r > ou igual rows significa que já temos o tabuleiro Completo


        case (r, c) if c >= cols => loop(r + 1, 0, acc) // próxima linha ( c >= cols ) significa que já chegamos ao final da linha atual.

        // preencher posição atual
        case (r, c) =>
          val stone = (r + c) % 2 match {
              case 0 => Stone.Black
              case _ => Stone.White
            }
          loop(r, c + 1, acc + ((r, c) -> stone)) //Seguir para a proxima posicao da linha.
      }
    loop(0, 0, ParMap.empty) //Basicamente é isto que vamos devolver
  }


  //Esta função devolve as peças Possiveis apartir de um player e uma coordenada. -> Muito Util.
  def validDestinationsFromPiece(board: Board, player: Stone, coordFrom: Coord2D, openCoords: List[Coord2D]): List[Coord2D] = {
    val possibleDestinations = listValidDestinations(board, player, openCoords)
    possibleDestinations.filter(coordTo => isValidPlay(board, player, coordFrom, coordTo, openCoords))
  }


  // função de jogada
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {

    if !isValidJump(coordFrom, coordTo) || !listContains(lstOpenCoords, coordTo) || !board.get(coordFrom).contains(player) then (None, lstOpenCoords)
    else
      val posicao = intermediateCoord(coordFrom, coordTo)
      val valor_na_posicao = board.get(posicao)
      if valor_na_posicao.isEmpty || valor_na_posicao.contains(player) then
        (None, lstOpenCoords)
      else
        val newboard = board - coordFrom - posicao + (coordTo -> player) // tiramos a posicao intermedia, a posicao incial e adicionamos a nova posicao á newboard
        val newlstOpencoords = coordFrom :: posicao :: removeCoord(lstOpenCoords, coordTo) //remover as coordenadas livres a posicao para onde nos movemos, e adicionamos a posicao ao final da lista

        (Some(newboard), newlstOpencoords)

  }

  def isValidJump(origin: Coord2D, destination: Coord2D): Boolean = {
    val (x1, y1) = origin //coordenadas da origem
    val (x2, y2) = destination //coordenadas destino
    val dx = x2 - x1 //calculo do salto
    val dy = y2 - y1
    //usamos a funcao já feita math.abs de modo a devolver os valores em positivo (os saltos podem ser (2,4) para (2,2) e ai iria dar (0,-2)
    (math.abs(dx), math.abs(dy)) match
      case (2, 0) | (0, 2) => true
      case _ => false
  } //Função que determina se um salto é válido

  def intermediateCoord(origin: Coord2D, destination: Coord2D): Coord2D =
    val (x1, y1) = origin //coordenadas da origem
    val (x2, y2) = destination //coordenadas destino
    ((x1 + x2) / 2, (y1 + y2) / 2)

  @tailrec
  def listContains(Lista: List[Coord2D], coordenada: Coord2D): Boolean = {

    Lista match {
      case Nil => false
      case h :: t => if h == coordenada then true
      else
        listContains(t, coordenada)
    }

  }

  @tailrec
  def removeCoord(xs: List[Coord2D], target: Coord2D, acc: List[Coord2D] = Nil): List[Coord2D] = {
    xs match
      case Nil => acc.reverse
      case h :: t =>
        if h == target then acc.reverse ::: t
        else removeCoord(t, target, h :: acc)
  }


  // função auxiliar para determinar se uma determinada posição é uma posição jogável por uma dada peça
  //Devolve se uma jogada é valida ou não
  def isValidPlay(board: Board, player: Stone, origin: Coord2D, destination: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {

    // vai verificar se o salto é válido
    if (!isValidJump(origin, destination)) then false
    else

      // calcula a posição intermédia sobre a qual vai saltar
      val middle = intermediateCoord(origin, destination)

      board.get(origin) match
        // verifica se é o player que está na posição de origem
        case Some(p) if p == player =>
          // verifica se é o adversário que está na posição intermédia (que vai ser comida)
          board.get(middle) match

            case Some(opponent) if opponent != player =>
              // a posição de destino tem de estar vazia, isto é, não pode estar contida no board
              listContains(lstOpenCoords, destination)

            case _ => false

        case _ => false
  }


  //T3

  def listPlayerCoords(board: Board, player: Stone): List[Coord2D] = {
    // transforma o board numa list, filtra pelas posições do jogador
    // e devolve a lista de coordenadas do jogador
    board.toList.filter(x => x._2 == player)
      .map(x => x._1)
  }


  // esta função verifica se um determinado destino é uma posição jogável para alguma das posições do jogador
  @tailrec
  def canPlayTo(board: Board, player: Stone, myCoords: List[Coord2D], coordTo: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {
    myCoords match
      case Nil => false
      case coordFrom :: tail =>
        if isValidPlay(board, player, coordFrom, coordTo, lstOpenCoords) then true
        else canPlayTo(board, player, tail, coordTo, lstOpenCoords)
  }


  //Aqui vamos devolver a lista de posições que podemos usar para jogar de acordo com a stone atual
  //Podemos usar de modo a ver se uma tal posição é jogavel ou não
  def listPlayablePositions(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): List[Coord2D] = {
    val myCoords = listPlayerCoords(board, player) //nossas coordenadas no tabuleiro

    @tailrec
    def loop(remaining: List[Coord2D], acc: List[Coord2D]): List[Coord2D] = {
      // remaining -> nossas posições no tabuleiro
      remaining match
        case Nil =>  acc.reverse //retornamos o acumulador(resultado)
        case coordFrom :: tail =>
          val hasMove = lstOpenCoords.exists(coordTo => play(board, player, coordFrom, coordTo, lstOpenCoords)._1.isDefined) //IMPORTANTE -> Perceber se podemos usar isto para o nosso projeto.
          if hasMove then
            loop(tail, coordFrom :: acc) //neste if basicamente encontramos uma posição jogavel, sendo assim iremos guardar esta
          else
            loop(tail, acc)
    }
    loop(myCoords, Nil) //Começamos por passar para o loop as posições que temos no nosso tabuleiro e Nil o nosso  acumulador que irá representar o resultado
  }



  // esta função constrói uma lista de posições jogáveis para as peças do jogador. //Posições Destino
  def listValidDestinations(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): List[Coord2D] = {

    val myCoords = listPlayerCoords(board, player) //Lista de Peças do jogador, de acordo com a stone

    @tailrec
    def loop(remaining: List[Coord2D], acc: List[Coord2D]): List[Coord2D] = {
      remaining match {
        case Nil => acc.reverse //chegamos ao Fim
        case coordTo :: tail =>
          if canPlayTo(board, player, myCoords, coordTo, lstOpenCoords) then
            loop(tail, coordTo :: acc) //Este if faz com que, caso a peça atual possa ser jogada, colocar na Lista de posições jogaveis
          else loop(tail, acc) //se não é jogavel vamos percorrer o resto da Lista
      }
    }

    loop(lstOpenCoords, Nil) //Passamos a Lista de posiçoes abertas -> E nil (acumulador)
  }

  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D], f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {

    // faz uma lista de coordenadas onde estão posicionadas as peças do jogador que está a jogar
    val myCoords = listPlayerCoords(board, player)

    // se não há peças, não faz nada
    if myCoords.isEmpty then (None, r, lstOpenCoords, None)
    //no caso em que há peças
    else

      // filtra a lista de posições vazias , ficando apenas com aquelas para as quais o jogar pode jogar,
      // numa jogada válida
      val validDestinations = listValidDestinations(board, player, lstOpenCoords)

      if validDestinations.isEmpty then
        (None, r, lstOpenCoords, None)
      else
        // escolhe aleatoriamente uma das coordenadas de destino correspondentes a uma jogada válida
        val (coordTo, r2) = f(validDestinations, r)

        // vai filtrar quais são as coordenadas das peças do jogador que podem mover-se para a posição de destino,
        // através de uma jogada válida
        val validOrigins = myCoords.filter(coordFrom =>
          isValidPlay(board, player, coordFrom, coordTo, lstOpenCoords))

        // escolhe as primeiras coordenadas que encontra da peça que se pode mover para o destino com uma jogada válida
        val coordFrom = validOrigins.head

        // move a peça da coordenada de origem para a de destino
        val (newBoard, newLstOpenCoords) = play(board, player, coordFrom, coordTo, lstOpenCoords)

        newBoard match
          case None => (None, r2, lstOpenCoords, None)
          case Some(newBoard) => (Some(newBoard), r2, newLstOpenCoords, Some(coordTo))
  }


// T5 implementar o método responsável por verificar se o computador ou o jogador
// ganhou o jogo


//Indica se o jogador tem alguma jogada valida ou não
  def hasValidMove(board: Board, player: Stone, lstOpenCoords: List[Coord2D]) : Boolean = {
    // faz uma lista de coordenadas onde estão posicionadas as peças do jogador que está a jogar
    val myCoords = listPlayerCoords(board, player)

    // se não há peças, não tem jogadas para fazer
    if myCoords.isEmpty then false
    else
      // filtra a lista de posições vazias, ficando apenas com aquelas para as quais o jogar pode jogar,
      // numa jogada válida
      val validDestinations = listValidDestinations(board, player, lstOpenCoords)

      // se não tem posições de destino válidas, não tem jogadas válidas
      !validDestinations.isEmpty
  }

  def isGameOver(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): Boolean = {
    // se o jogador já não tem jogadas válidas para fazer, é Game Over
    !hasValidMove(board, player, lstOpenCoords)
  }

  // T6 adicionar um temporizador limite (configurável no início do jogo) para cada
  // jogada e, permitir que seja possível após cada jogada realizar undo, i.e., anular a
  // última movimentação do jogador e do computador
  def undoMove(history: GameHistory): Option[(GameState, GameHistory)] = {
    history match {
      case Nil => None
      case lastState :: tail => Some((lastState, tail))
    }
  }

  def isTimeExceeded(startTime: Long, timeLimit: Long): Boolean = {
    val currentTime = System.currentTimeMillis()
    currentTime - startTime > timeLimit
  }




  //Devolve as peças clicaveis ou escolhiveis na fase do jogo que estamos, exemplo de uso: caso estejamos na fase da primeira remoção da peça devolvemos apenas as peças possiveis de primeira remoção, ou seja não temos nem em conta o select
  def getValidInteractions(state: GameState, phase: Phase, selected: Option[Coord2D]): List[Coord2D] =
    phase match
      case Phase.InitialRemoval =>
        FistPieceToRemove(6,6) // Substituir por lógica dinâmica se o tamanho mudar --> Fazer função depois receber mais contexto sobre tamamanho, ter em conta que aqui vamos receber o tabuleiro "cheio"

      case Phase.SecondRemoval =>
        // Apenas as posições adjacentes à primeira peça removida
        state._3 match
          case Nil => Nil //Possivel erro na retirada da primeira peça
          case h :: t => Konane.AdjPieces(h) //Pode ser mais dinamico -> Depender do Tamanho do tabuleiro (Possivel Option Aqui)
      case Phase.Playing =>
        selected match
          case None =>
            // Nenhuma peça selecionada: devolve todas as peças do jogador atual que têm movimentos válidos
            listPlayablePositions(state._1, state._2, state._3) //Lista de jogaveis
          case Some(fromPiece) => //Aqui no estado Playing consideramos que queremos realizar uma jogada
            // devolvemos apenas os destinos válidos para ESTA peça
            validDestinationsFromPiece(state._1,state._2,fromPiece,state._3)
      case Phase.Capturing =>
        selected match
          case Some(currentPos) =>
            // Em captura múltipla, só destinos válidos a partir da peça que saltou
            validDestinationsFromPiece(state._1, state._2, currentPos, state._3) :+ currentPos //Adicionamos também a mesma peça
          case None => //Não é valido
            Nil

  def processInteraction(contx: GameContext, inputCoord: Coord2D): Option[GameContext] = {
    val validOptions = getValidInteractions(contx.state, contx.phase, contx.selected) //Aqui devolve oq é jogavel

    if (!listContains( validOptions,inputCoord)) {None }

    // Interação inválida, não faz nada, não mudança de estados e etc.
    else
      contx.phase match { //Com a validaçaõ que é "válido" o jogo temos que dar contexto ás peças recebidas e agir de acordo com isso.
        case Phase.InitialRemoval =>
          val newBoard = Konane.removePecas(contx.state._1, List(inputCoord)) //Estamos na fase em que vamos remover a primeira peça, passamos a peça dada pelo utilizador (já validada pelo validOption)
          val newOpenList = contx.state._3 :+ inputCoord //Adicionamos a nova peça
          // Fazemos switch do Player, Peça Branca que irá remover peça.
          val newState = (newBoard, switchPlayer(contx.state._2), newOpenList)
          Some(contx.copy(state = newState, phase = Phase.SecondRemoval)) //Devolvemos novo contexto de jogo,  com novo estado e fase de jogo

        case Phase.SecondRemoval => //Phase de 2ºRemoção. //Nao guardar o historico.
          val newBoard = Konane.removePecas(contx.state._1, List(inputCoord)) //nova remoção.
          val newOpenList = contx.state._3 :+ inputCoord //novo lista de posições jogaveis
          // Segunda peça removida -> Passa o turno para o próximo jogador e entra em Playing
          val newState = (newBoard, switchPlayer(contx.state._2), newOpenList) //Trocamos de player, entramos no "real Game"
          Some(contx.copy(state = newState, phase = Phase.Playing)) //Novo contexto

        case Phase.Playing => //Phase de Jogo (aqui não estamos a incluir a multipla captura)
          contx.selected match { //Aqui vamos ver se já temos alguma coordenada Selecionada ou não
            case None =>
              // Selecionou a peça de origem -> guarda a seleção, mantemos a fase
              Some(contx.copy(selected = Some(inputCoord)))

            case Some(from) => //No caso de termos algo já selecionado, indica que vamos realizar uma jogada -> O validOption garante isso.

              val (newBoardOpt, newOpenLista) = Konane.play(contx.state._1, contx.state._2, from, inputCoord, contx.state._3) //O inputCoord será o Destino.
              newBoardOpt.map { board =>
                // Verifica se a peça que acabou de aterrar consegue fazer mais saltos
                val canContinue = validDestinationsFromPiece(board, contx.state._2, inputCoord, newOpenLista).nonEmpty

                if (canContinue) {
                  val newState = (board, contx.state._2, newOpenLista)
                  contx.copy(state = newState, phase = Phase.Capturing, selected = Some(inputCoord))
                } else {
                  val nextHistory = contx.state :: contx.history //Adicionamos o GameState Antigp à historia do jogo //Atenção Aqui.
                  val newState = (board, switchPlayer(contx.state._2), newOpenLista)
                  contx.copy(state = newState, phase = Phase.Playing, selected = None, history = nextHistory) //Aqui guardar o historico
                }
              }
          }

        case Phase.Capturing =>
          // O jogador pode carregar na própria peça para "parar" a captura, ou escolher um novo destino
          if (contx.selected.get == inputCoord) {
            // Finalizou voluntariamente a captura múltipla -> roda o turno
            val nextHistory = contx.state :: contx.history
            val newState = (contx.state._1, switchPlayer(contx.state._2), contx.state._3)
            Some(contx.copy(state = newState, phase = Phase.Playing, selected = None , history = nextHistory))
          } else {


            // Executa o próximo salto da sequência
            val from = contx.selected.get //Melhorar Aqui!
            val (newBoardOpt, newOpen) = Konane.play(contx.state._1, contx.state._2, from, inputCoord, contx.state._3)
            newBoardOpt.map { board =>
              val canContinue = validDestinationsFromPiece(board, contx.state._2, inputCoord, newOpen).nonEmpty //O canContinue vai basicamente ditar se saimos/continuamos do modo de Capturing(captura multipla).
              if (canContinue) {
                val newState = (board, contx.state._2, newOpen) //o GameState vai continuar com o mesmo Player, novo tabuleiro e nova posiçoes Abertas.
                contx.copy(state = newState, selected = Some(inputCoord)) //O selected passa a ser o InputCoord2D (para onde jogamos - onde estamos).
              } else {
                val nextHistory = contx.state :: contx.history
                val newState = (board, switchPlayer(contx.state._2), newOpen) //trocamos de jogador
                contx.copy(state = newState, phase = Phase.Playing, selected = None , history = nextHistory) //não temos nada selecionado agora, e voltamos ao estado de playing(jogadas normais).
              }
            }
          }
      }
  }

