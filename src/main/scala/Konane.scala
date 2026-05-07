import com.sun.source.tree.WhileLoopTree

import scala.collection.parallel.immutable.ParMap
import scala.annotation.tailrec

object Konane:

  type Coord2D = (Int, Int)

  type Board = ParMap[Coord2D, Stone]

  enum Stone:
    case Black, White


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
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D], f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {

    // faz uma lista de coordenadas onde estão posicionadas as peças do jogador que está a jogar
    val myCoords = board.toList.filter(x => x._2 == player).map(x => x._1)

    // se não há peças, não faz nada
    if myCoords.isEmpty then (None, r, lstOpenCoords, None)
    //no caso em que há peças
    else

      // filtra a lista de posições vazias , ficando apenas com aquelas para as quais o jogar pode jogar,
      // numa jogada válida
      val validDestinations = lstOpenCoords.filter(coordTo =>
        myCoords.exists(coordFrom =>
          isValidPlay(board, player, coordFrom, coordTo, lstOpenCoords)
        )
      )

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


  //T4
  // Converte Stone para Char
  def stoneToChar(stone: Stone): Char = stone match {
    case Stone.Black => 'B'
    case Stone.White => 'W'}

  // Buscamos na board, passando a board (tabuleiro) e depois a coordenada.
  def getCelula(board: Board, coord: Coord2D): Char = {
    board.get(coord) match
      case Some(stone) => stoneToChar(stone)
      case None => '.'
  }

  // Descobre tamanho máximo assumindo que o é tabuleiro quadrado
  def boardSize(board: Board): Int = {
    board.keys.map((r, c) => math.max(r, c)).max + 1 //assumimos também que aqui as posições esta bem ordenadas, ou seja , não vamos ter um Black, (10,10), enquanto o tamanho do tabuleiro é 5 x 5.
  }

  // Gera header (A B C D ...) , como no exemplo do enunciado
  def printHeader(size: Int): Unit = {
    @tailrec
    def loop(col: Int): Unit =
      if col < size then //basicamente vamos percorrer as letras de acordo com o tamanho do tabuleiro.
        print(s"${('A' + col).toChar} ")
        loop(col + 1)
      else println()

    print("  ")
    loop(0)
  }

  // Gera uma linha
  def printRow(board: Board, row: Int, size: Int): Unit = {
    @tailrec
    def loop(col: Int): Unit =
      if col < size then
        val celula = getCelula(board, (row, col)) //aqui vamos percorrer os outros elementos da coluna, ou seja já estamos na linha.
        print(s"$celula ")
        loop(col + 1)
      else println()

    print(s"$row ")
    loop(0)
  }

  // Função principal que vai chamar as outras auxiliares
  def printBoard(board: Board): Unit = {
    val size = boardSize(board) //calculamos o tamanho da board.

    printHeader(size) //fazemos print do header(cabecalho) como na imagem do enunciado

    @tailrec
    def loop(row: Int): Unit =
      if row < size then //aqui percorremos linha a linha, as outras funções vao percorrer coluna a coluna.
        printRow(board, row, size)
        loop(row + 1)

    loop(0)
  }

// T5 implementar o método responsável por verificar se o computador ou o jogador
//ganhou o jogo.
  
  def hasValidMove(board: Board, player: Stone, lstOpenCoords: List[Coord2D]) : Boolean = {
    
  }
  
  def isGameOver(board: Board, player: Stone): Boolean = {
    
  }




