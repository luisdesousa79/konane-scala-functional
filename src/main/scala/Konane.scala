import com.sun.source.tree.WhileLoopTree

import scala.collection.parallel.immutable.ParMap
import scala.annotation.tailrec

object Konane:

  type Coord2D = (Int, Int)

  type Board = ParMap[Coord2D, Stone]

  enum Stone:
    case Black, White
    
  type GameState = (Board, Stone, List[Coord2D])
  
  type GameHistory = List[GameState]


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

    //devolve coordenada e
    (coord, newRandState)
  }

  def removePecas(board: Board, removed: List[Coord2D]): Board =

    removed.foldLeft(board) {
      case (b, coord) => b - coord}


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
  }

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



  // esta função constrói uma lista de posições jogáveis para as peças do jogador.
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



