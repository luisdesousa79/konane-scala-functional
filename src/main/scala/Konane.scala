import scala.collection.parallel.immutable.ParMap
import scala.annotation.tailrec

trait RandomWithState {
  def nextInt(): (Int, RandomWithState)
  def nextInt(n: Int): (Int, RandomWithState)
}

case class MyRandom(seed: Long) extends RandomWithState {
  def nextInt(): (Int, RandomWithState) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRandom)
  }

  def nextInt(n: Int): (Int, RandomWithState) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val nn = ((newSeed >>> 16).toInt) % n
    (if (nn < 0) -nn else nn, nextRandom)
  }
}

object Konane:

  type Coord2D = (Int, Int)

  type Board = ParMap[Coord2D, Stone]

  enum Stone: 
    case Black, White

  
  
  // função que implementa um movimento aleatório
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    // se não há coordenadas para onde jogar lança excepção
    if(lstOpenCoords.isEmpty) { 
      // aqui devíamos ter uma maneira de dizer que o jogador perdeu o jogo, em vez de lançar uma excepção
      throw new Exception("não há posições para onde jogar!")
    } else
      // caso contrário, escolhe aleatoriamente um índice da lista de coordenadas vazias
      // atenção que isto não procura posições jogáveis (Adjacentes) - isso é feito pela função play
        val (randomIndex, newRand) = rand.nextInt(lstOpenCoords.length)
        val coord = lstOpenCoords(randomIndex)
        (coord, MyRandom(10)) //COMENT IMPortante, modifiquei aqui.

  }

  // função que inicializa o tabuleiro
  // incompleta , Tenho que fazer
  def initBoard(n: Int): Board = {

    @tailrec
    def loop(row: Int, col: Int, acc: ParMap[Coord2D, Stone]): ParMap[Coord2D, Stone] =
      (row, col) match {

        // Caso de paragem, caso r já esteja superior a n significa que já preenchemos o tabuleiro
        case (r, _) if r >= n => acc

        // Próxima linha, c já é maior que n ou seja vamos para a proxima linha
        case (r, c) if c >= n =>
          loop(r + 1, 0, acc)

        // Caso normal, basicamente vamos adicionando c(incrementando) começando com ele a 0 na chamado abaixo loop(0,0), de acordo com as nossas regras se % 2 == 0 é uma peça(preta) , se nãé outro tipo de peça(branca)
        case (r, c) =>
          val stone = (r + c) % 2 match { //val stone , valor que guarda de que cor é a peça que queremos
            case 0 =>
              //println((r, c))
              //println("Preta")
              Stone.Black

            case _ =>
              //println((r, c))
              //println("Branca")
              Stone.White
          }

          loop(r, c + 1, acc + ((r, c) -> stone)) //atribuimos mais 1 ao c ( de modo a preencher toda a linha). Acc vai ser o acumulador.
      }

    (loop(0, 0, ParMap.empty))

  }

  @main
  def main(): Unit = {

    val re = initBoard(5)
    re.foreach { case ((row, col), stone) =>
      println(s"($row,$col) -> $stone")
    }


  }
  
  
  // função auxiliar para determinar se uma determinada posição é uma posição jogável para uma peça dada
  def isValidPlay(board: Board, player: Stone, origin: Coord2D, destination: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {
      
      // vai buscar as coordenadas de origem e destino
      val (x1, y1) = origin
      val (x2, y2) = destination
      
      // calcula a direcção
      val dx = x2 - x1
      val dy = y2 - y1
      
      // define um valor booleano que representa uma direcção válida de movimento
      val validDirection  = (math.abs(dx) == 2 && dy == 0) || (math.abs(dy) == 2 && dx == 0)

      if (!validDirection) then false
      else
        // calcula a posição intermédia sobre a qual vai saltar
        val middle =((x1 + x2) / 2, (y1 + y2)/2)

        board.get(origin) match
          // verifica se é o player que está na posição de origem 
          case Some(p) if p == player =>
          // verifica se é o adversário que está na posição intermédia (que vai ser comida)
            board.get(middle) match
              case Some(opponent) if opponent != player =>
                // a posição de destino tem de estar vazia, isto é, não pode estar contida no board
                lstOpenCoords.contains(destination)

              case _ => false

          case _ => false
  }
  
  // função de jogada
  // incompleta!!! //tenho que fazer
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, 
           lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) =
    //if (isValidPlay(board, player, coordFrom, coordTo)) then
      
    //else
      (None, lstOpenCoords)
    
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)):
                    (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) =

    // faz uma lista de coordenadas onde estão posicionadas as peças do jogador que está a jogar
    val myCoords = board.toList.filter(x => x._2 == player).map(x => x._1)

    // se não há peças, não faz nada
    if myCoords.isEmpty then (None, r, lstOpenCoords, None)
      //no caso em que há peças
    else

      // escolhe aleatoriamente uma coordenada de origem
      val (coordFrom, newRand) = f(myCoords, r)
      
      // escolhe coordenada de destino aleatoriamente

      //val validDest = lstOpenCoords.filter(x => isValidPlay(x))
      val (coordTo, newRand2) = f(lstOpenCoords, newRand)

      // move a peça da coordenada de origem para a de destino
      val (newBoard, newLstOpenCoords) = play(board, player, coordFrom, coordTo, lstOpenCoords)

      newBoard match
        case None => (None, newRand2, lstOpenCoords, None)
        case Some(newBoard) => (Some(newBoard), newRand2, newLstOpenCoords, Some(coordTo))




