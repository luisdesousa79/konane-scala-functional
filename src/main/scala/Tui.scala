import Konane.{Board, Stone}

// Case class responsável pela Interface de Texto do Usuário (Text User Interface)
// É uma case class porque não precisa de estado mutável e ganha métodos úteis automaticamente
case class Tui() {

  // Função auxiliar que converte uma lista de pedras (Option[Stone]) em uma string visual
  // Exemplo: List(Some(Black), None, Some(White)) -> "B . W "
  def auxTUI(lst: List[Option[Stone]]): String = lst match {
    // CASO BASE: lista vazia -> retorna string vazia
    case Nil => ""

    // CASO RECURSIVO: pega a primeira pedra (x) e o resto (xs)
    case x :: xs =>
      // Verifica qual é a pedra atual e converte para um caractere
      (x match {
        case Some(Stone.Black) => "B "   // Pedra preta -> letra B
        case Some(Stone.White) => "W "   // Pedra branca -> letra W
        case None => ". "                // Posição vazia -> ponto final
      }) + auxTUI(xs)  // Chama recursivamente para o resto da lista
  }

  // Função principal que mostra o tabuleiro completo na tela
  // Recebe um Option[Board] porque o tabuleiro pode estar vazio (None)
  def mostrar(board: Option[Board]): Unit =
    // Primeiro, descobre as dimensões do tabuleiro chamando uma função auxiliar
    // getBoardSize retorna (cols, rows) - número de colunas e linhas
    val (cols, rows) = Konane.getBoardSize(board)

    // Verifica se o tabuleiro existe
    board match {
      case Some(a) =>  // Tabuleiro existe (Some)

        // Função interna recursiva que imprime o cabeçalho das colunas
        // Exemplo: "  A B C D E"
        def printHeader(col: Int): Unit = col match {
          // CASO BASE: já imprimiu todas as colunas
          case c if c == cols => println()  // Pula para a próxima linha

          // CASO RECURSIVO: ainda tem colunas para imprimir
          case c =>
            val letra = ('A' + c).toChar  // 0->'A', 1->'B', 2->'C', etc.
            print(s" $letra")              // Imprime " A", " B", " C"...
            printHeader(c + 1)             // Chama para a próxima coluna
        }

        print("  ")      // Espaçamento inicial para alinhar com os números das linhas
        printHeader(0)   // Começa a imprimir o cabeçalho da coluna 0

        // Chama a função recursiva para imprimir as linhas do tabuleiro
        mostrarRec(board, rows, cols, 0)

      case None =>  // Tabuleiro não existe (None)
        println("não é válido")  // Mensagem de erro
    }

  // Função recursiva que imprime cada linha do tabuleiro
  // Parâmetros:
  // - board: tabuleiro opcional
  // - rows: número total de linhas
  // - cols: número total de colunas
  // - currentRow: linha atual que está sendo impressa (começa em 0)
  def mostrarRec(board: Option[Board], rows: Int, cols: Int, currentRow: Int): Unit = currentRow match {
    // CASO BASE: já imprimiu todas as linhas
    case r if r == rows => ()  // Não faz nada, termina a recursão

    // CASO RECURSIVO: ainda tem linhas para imprimir
    case r =>
      // Imprime o número da linha com formatação de 2 dígitos
      // Exemplo: " 1 ", " 2 ", " 3 "...
      print(String.format("%2d ", r + 1))

      // Verifica se o tabuleiro existe
      board match {
        case Some(a) =>  // Tabuleiro existe
          // Cria uma lista com todas as pedras da linha atual (r)
          // Para cada coluna (y de 0 até cols-1), pega a pedra na posição (r, y)
          val linha = (0 until cols).map(y => a.get((r, y))).toList

          // Imprime a linha usando a função auxTUI para converter pedras em caracteres
          println(auxTUI(linha))

          // Chama recursivamente para a próxima linha (r + 1)
          mostrarRec(board, rows, cols, r + 1)
      }
  }
}