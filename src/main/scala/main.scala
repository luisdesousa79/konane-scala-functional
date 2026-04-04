class main {
  
    val board = Map(
      (2, 2) -> Konane.Stone.Black,
      (3, 2) -> Konane.Stone.White
    )

    val lstOpen = List((4, 2))

    val rand = MyRandom(42)

    val result =
      Konane.playRandomly(board, rand, Konane.Stone.Black, lstOpen, Konane.randomMove)

    println(result)

}
