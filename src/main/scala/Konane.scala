import scala.collection.parallel.immutable.ParMap

object Konane:

  type Coord2D = (Int, Int)

  type Board = ParMap[Coord2D, Stone]

  enum Stone: 
    case Black, White
    
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, 
           lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D])
      ???
    
  def playRandomly(board: Board, r: MyRandom, player: Stone, 
                   lstOpenCoords: List[Coord2D], f: (List[Coord2D], MyRandom) => 
    (Coord2D, MyRandom)): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D])

    val (coord, newRand) = f(lstOpenCoords, r)
      
    val (newBoard, newLstOpenCoords) = play(board, player, lstOpenCoords)
      
      ???


