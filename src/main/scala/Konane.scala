import scala.collection.parallel.immutable.ParMap

object Konane:

  type Coord2D = (Int, Int)

  type Board = ParMap[Coord2D, Stone.stone]

  
  // função que implementa um movimento aleatório
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    // se não há coordenadas para onde jogar lança excepção
    if(lstOpenCoords.isEmpty) { 
      // aqui devíamos ter uma maneira de dizer que o jogador perdeu o jogo, em vez de lançar uma excepção
      throw new Exception("não há posições para onde jogar!")
    } else
      // caso contrário, escolhe aleatoriamente um índice da lista de coordenadas vazias
      // atenção que isto não procura posições jogáveis (Adjacentes) - isso é feito pela função play
        val (randomIndex, newRand) = rand.nextInt1(lstOpenCoords.length)
      
        val newRandState = newRand.asInstanceOf[MyRandom]      
        val coord = lstOpenCoords(randomIndex)
        (coord, newRandState)
  }


  // função que inicializa o tabuleiro
  // incompleta
  def initBoard(n: Int) = ???
  
  
  // função auxiliar para determinar se uma determinada posição é uma posição jogável para uma peça dada
  def isValidSalto(origin: Coord2D, destination: Coord2D): Boolean = {
    val (x1, y1) = origin //coordenadas da origem
    val (x2, y2) = destination //coordenadas destino
    val dx = x2 - x1 //calculo do salto
    val dy = y2 - y1
    //usamos a funcao já feita math.abs de modo a devolver os valores em positivo (os saltos podem ser (2,4) para (2,2) e ai iria dar (0,-2)
    (math.abs(dx), math.abs(dy)) match
      case (2, 0) | (0, 2) => true
      case _ => false

  }

  def ListaCointains(Lista: List[Coord2D], coordenada: Coord2D): Boolean = {

    Lista match {
      case Nil => false
      case h :: t => if h == coordenada then true
      else
        ListaCointains(t, coordenada)

    }

  }

  def removeCoordenada(xs: List[Coord2D], target: Coord2D, acc: List[Coord2D] = Nil): List[Coord2D] = {
    xs match
      case Nil => acc.reverse
      case h :: t =>
        if h == target then acc.reverse ::: t
        else removeCoordenada(t, target, h :: acc)
  }

  def CoordIntermedio(origin: Coord2D, destination: Coord2D): Coord2D =
    val (x1, y1) = origin //coordenadas da origem
    val (x2, y2) = destination //coordenadas destino
    ((x1 + x2) / 2, (y1 + y2) / 2)


  // função de jogada
  // incompleta!!!
  def play(board: Board, player: Stone.stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {

    if !isValidSalto(coordTo, coordFrom) || !ListaCointains(lstOpenCoords, coordTo)  || !board.get(coordFrom).contains(player) then (None, lstOpenCoords)
    else
      val posicao = CoordIntermedio(coordTo,coordFrom)
      val valor_na_posicao = board.get(posicao)
      if valor_na_posicao.isEmpty || valor_na_posicao.contains(player) then
        (None, lstOpenCoords)
      else
        val newboard = board - coordFrom - posicao + (coordTo -> player) // tiramos a posicao intermedia, a posicao incial e adicionamos a nova posicao á newboard
        val newlstOpencoords = coordFrom :: posicao :: removeCoordenada(lstOpenCoords, coordTo) //remover as coordenadas livres a posicao para onde nos movemos, e adicionamos a posicao ao final da lista

        (Some(newboard), newlstOpencoords)

  }

    
  def playRandomly(board: Board, r: MyRandom, player: Stone.stone, lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)):
                    (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) =

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
          isValidSalto(coordFrom, coordTo) 
        ) 
      )
      
        
      if validDestinations.isEmpty then
        (None, r, lstOpenCoords, None)
      else
        // escolhe aleatoriamente uma das coordenadas de destino correspondentes a uma jogada válida
        val (coordTo, r2) = f(validDestinations, r)
      
        // vai filtrar quais são as coodenadas das peças do jogador que podem mover-se para a posiçãod e destino,
        // através de uma jogada válida
        val validOrigins = myCoords.filter(coordFrom => 
        isValidSalto(coordFrom, coordTo))
        
        // escolhe as primeiras coordenadas que encontra da peça que se pode mover para o destino com uma jogada válida
        val coordFrom = validOrigins.head

        // move a peça da coordenada de origem para a de destino
        val (newBoard, newLstOpenCoords) = play(board, player, coordFrom, coordTo, lstOpenCoords)

        newBoard match
          case None => (None, r2, lstOpenCoords, None)
          case Some(newBoard) => (Some(newBoard), r2, newLstOpenCoords, Some(coordTo))




