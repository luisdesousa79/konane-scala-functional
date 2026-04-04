trait RandomWithState {

    def nextInt: (Int, RandomWithState)

    def nextInt1(n: Int): (Int, RandomWithState)

  }