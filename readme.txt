## README – Parte 1

A implementação desta primeira fase obrigou-nos a fazer algumas escolhas com base na interpretação que fizemos do enunciado.

## T1 (randomMove)

Assim, na função T1 (randomMove), consideramos um *randomMove* como a escolha aleatória de uma posição a partir da lista de coordenadas vazias.

Ou seja, dado um conjunto de posições disponíveis, a função seleciona uma delas de forma aleatória e devolve essa posição (destino), juntamente com o novo estado do gerador aleatório.

---

### T2 (play)

Aqui fizemos duas funções, init board, e play. Em init board usamos um valor inteiro como parametro que considerando que a Board irá ser quadrada sempre, 
e recebemos uma lista de Coord2D que basicamente  ira remover as duas posições iniciais como é feito no jogo konane.

Na funcao play, foram usadas outras funcoes auxiliares como isValidJump , de modo a garantir que as jogadas podem ser efetuadas.


---

## T3 (playRandomly)

Já na função T3 (playRandomly), optámos por implementar a jogada da seguinte forma:

* Filtramos a lista de coordenadas vazias, ficando apenas com as posições que são jogáveis para o jogador atual.
* Se não existir nenhuma posição jogável, não é feita qualquer jogada.
* Caso contrário, escolhemos uma dessas posições aleatoriamente (usando o T1).

Depois de escolhido o destino:

* Construímos a lista das peças do jogador que podem mover-se para essa posição.
* No caso desta lista ter mais de um elemento, escolhemos a primeira da lista para efetuar a jogada.

Assim, a aleatoriedade está na escolha do destino, sendo a escolha da peça dependente dessa escolha — ou seja, indiretamente aleatória.



