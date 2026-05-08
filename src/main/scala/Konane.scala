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


  //T2 - Funcao Initboard e Play
  // função que inicializa o tabuleiro
  def initBoard(n: Int, removed: List[Coord2D]): Board = {

    @tailrec
    def loop(row: Int, col: Int, acc: Board): Board =
      (row, col) match {

        // Caso de paragem, caso r já esteja superior a n significa que já preenchemos o tabuleiro
        case (r, _) if r >= n => acc

        // Próxima linha, c já é maior que n ou seja vamos para a próxima linha
        case (r, c) if c >= n =>
          loop(r + 1, 0, acc)

        // Caso normal, basicamente vamos adicionando c(incrementando) começando com ele a 0 na chamado abaixo loop(0,0), de acordo com as nossas regras se % 2 == 0 é uma peça(preta) , se não, é outro tipo de peça(branca)
        case (r, c) =>
          val stone = (r + c) % 2 match { //val stone , valor que guarda de que cor é a peça que queremos
            case 0 =>
              Stone.Black

            case _ =>
              Stone.White
          }

          loop(r, c + 1, acc + ((r, c) -> stone)) //atribuimos mais 1 ao c ( de modo a preencher toda a linha). Acc vai ser o acumulador.
      }
    val board = (loop(0, 0, ParMap.empty))
    removed.foldLeft(board)((b, coord) => b - coord)
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
  def canPlayTo(board: Board, player: Stone, myCoords: List[Coord2D], coordTo: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {
    myCoords match
      case Nil => false
      case coordFrom :: tail =>
        if isValidPlay(board, player, coordFrom, coordTo, lstOpenCoords) then true
        else canPlayTo(board, player, tail, coordTo, lstOpenCoords)
  }

  // esta função constrói uma lista de posições jogáveis para as peças do jogador.
  def listValidDestinations(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): List[Coord2D] = {
    val myCoords = listPlayerCoords(board, player)

    @tailrec
    def loop(remaining: List[Coord2D], acc: List[Coord2D]): List[Coord2D] = {
      remaining match {
        case Nil => acc.reverse
        case coordTo :: tail =>
          if canPlayTo(board, player, myCoords, coordTo, lstOpenCoords) then
            loop(tail, coordTo :: acc)
          else loop(tail, acc)
      }
    }

    loop(lstOpenCoords, Nil)
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
  //T7
  // Função para salvar estado atual (pode ser usada antes de jogadas importantes)
  // Função para salvar o estado atual do jogo no histórico
  // Recebe o estado atual e o histórico existente
  // Retorna um novo histórico com o estado atual adicionado no início
  def salvarEstadoAtual(estado: GameState, history: GameHistory): GameHistory = {
    estado :: history // Adiciona o estado atual à frente da lista
  }

  // Função para reiniciar o jogo
  // Recebe o histórico de estados
  // Retorna o estado mais antigo (primeiro estado do jogo)
  def reiniciar(history: GameHistory): Option[GameState] = {
    history.lastOption // lastOption pega o último elemento da lista (o mais antigo)
  }

  // Função que escolhe a dificuldade do jogo
  // dificuldade: 1 = Fácil (aleatório), 2 = Normal (prioriza centro)
  def jogarComDificuldade(
                           dificuldade: Int, // Nível de dificuldade escolhido
                           board: Board, // Tabuleiro atual
                           rand: MyRandom, // Gerador aleatório
                           player: Stone, // Jogador atual (Preto ou Branco)
                           lstOpenCoords: List[Coord2D] // Posições vazias
                         ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {

    dificuldade match {
      case 1 => // Fácil - jogada completamente aleatória
        Konane.playRandomly(board, rand, player, lstOpenCoords, Konane.randomMove)

      case 2 => // Normal - prioriza jogadas que vão para o centro
        Konane.jogadaNormal(board, rand, player, lstOpenCoords)

      case _ => // Padrão: fácil
        Konane.playRandomly(board, rand, player, lstOpenCoords, Konane.randomMove)
    }
  }

  // Função para jogada normal (prioriza o centro do tabuleiro)
  def jogadaNormal(
                    board: Board, // Tabuleiro atual
                    rand: MyRandom, // Gerador aleatório
                    player: Stone, // Jogador atual
                    lstOpenCoords: List[Coord2D] // Posições vazias
                  ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {

    // Lista de destinos válidos para o jogador
    val validDestinations = listValidDestinations(board, player, lstOpenCoords)

    // Se não há destinos, retorna None (não pode jogar)
    if (validDestinations.isEmpty) {
      return (None, rand, lstOpenCoords, None)
    }

    // Calcula o centro do tabuleiro
    val size = boardSize(board)
    val centro = (size / 2, size / 2)

    // Escolhe o destino mais próximo do centro
    val melhorDestino = validDestinations.minBy { case (x, y) =>
      math.abs(x - centro._1) + math.abs(y - centro._2) // Distância Manhattan
    }

    // Lista de coordenadas das peças do jogador
    val myCoords = listPlayerCoords(board, player)

    // Encontra a peça que pode se mover para o melhor destino
    val origem = myCoords.find(coordFrom =>
      isValidPlay(board, player, coordFrom, melhorDestino, lstOpenCoords)
    ).get

    // Executa a jogada
    val (newBoard, newOpen) = play(board, player, origem, melhorDestino, lstOpenCoords)

    // Retorna o novo tabuleiro e as novas posições vazias
    newBoard match {
      case Some(b) => (Some(b), rand, newOpen, Some(melhorDestino))
      case None => (None, rand, lstOpenCoords, None)
    }
  }

  // Função que calcula todas as posições vazias do tabuleiro
  // board: tabuleiro atual
  // n: tamanho do tabuleiro (número de linhas/colunas)
  def calcularPosicoesVazias(board: Board, n: Int): List[Coord2D] = {

    // Função recursiva que percorre todas as coordenadas
    @annotation.tailrec
    def loop(i: Int, j: Int, acc: List[Coord2D]): List[Coord2D] = {
      (i, j) match {
        case (i, _) if i >= n => acc // Terminou todas as linhas
        case (i, j) if j >= n => loop(i + 1, 0, acc) // Passa para próxima linha
        case (i, j) =>
          val coord = (i, j)
          // Verifica se a coordenada está vazia (não contém pedra)
          board.get(coord) match {
            case None => loop(i, j + 1, coord :: acc) // Vazia: adiciona à lista
            case Some(_) => loop(i, j + 1, acc) // Ocupada: não adiciona
          }
      }
    }

    loop(0, 0, List())
  }

  // Função que calcula o tamanho do tabuleiro (assumindo que é quadrado)
  def boardSize(board: Board): Int = {
    // foldLeft percorre todas as posições do tabuleiro
    // Começa com (0,0) e vai atualizando os máximos
    val (maxR, maxC) = board.foldLeft((0, 0)) {
      // Para cada posição ((r,c), _), atualiza os máximos
      case ((maxR, maxC), ((r, c), _)) => (math.max(maxR, r), math.max(maxC, c))
    }
    // O tamanho é o maior valor + 1 (porque as coordenadas começam em 0)
    math.max(maxR, maxC) + 1
  }


  


