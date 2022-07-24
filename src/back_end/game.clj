(ns back-end.game
  (:require [card-ascii-art.core :as card])) ;Estamos importando a biblioteca "card-ascii-art.core".

;Função que gera uma carta. Essas cartas podem ser:
;A, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K

;Cada uma dessas cartas serão representadas, internamente, pelos
;números "1...13"

(defn new-card ;Em Clojure, não é recomendado que as funções sejam nomeadas como verbos.
  "Essa função gerará uma nova carta de 1 a 13"
  []
  (inc ( rand-int 13))) ;A função "rand-int" gerará um número de 0 a 12, pois o "13" não é inclusivo. Assim, ao adicionarmos "1" ao resultado dessa função, será gerado um valor de "1" a "13".

(defn substituicao-JQK-por-10
  "Essa função substituirá os valores 'J', 'Q' e 'K' por 10, pois eles valerão 10 pontos."
  [valor-da-carta]
  (if (> valor-da-carta 10) 10 valor-da-carta)) ;Se a pontuação da carta for maior do que "10", será retornado o valor "10", senão, será retornado o valor da carta.

(defn substituicao-A-por-11
  "Essa função substituirá o Às pelo valor 11."
  [carta]
  (if (= carta 1) 11 carta))

;Essa função calculará os pontos de acordo com as cartas.
;As regras para o cálculo de pontos são as seguintes:
;   1. O "J", "Q" e o "K" valerão 10 pontos.
;   2. O "Às" valerá a melhor pontuação para o jogador, assim, por
;      exemplo, se o jogador tiver [Q, A], o "Às" valerá como "11", pois
;      ele ficará mais perto do "21", porém, se ele tiver "[Q, Q, A]", o
;      "Às" valerá como "1", pois se o "Às" valer como "11", o jogador estourará a pontuação.
(defn cards-points
  "Essa função receberá as cartas dos jogadores e calculará os pontos de cada jogador."
  [cards]
  (let [cards-sem-JQK (map substituicao-JQK-por-10 cards) ;O valor de "cards-sem-JQK", que é um vetor, será o retorno da função "substituicao-JQK-por-10", que será aplicada a todos os elementos do vetor "cards".
        cartas-com-A-valendo-11 (map substituicao-A-por-11 cards-sem-JQK) ;Para cada elemento do vetor "cards-sem-JQK", aplicaremos a função "substituicao-a-por-11", assim, teremos as cartas se o A valer "11".
        pontos-com-A-valendo-1 (reduce + cards-sem-JQK) ;O "pontos-com-A-valendo-1" será os pontos caso o "A" tenha o valor de "1". Ele será calculado em cima dos elementos do "cards-sem-JQK", pois, nesse caso, o Às valerá "11".
        pontos-com-A-valendo-11 (reduce + cartas-com-A-valendo-11)] ;O "pontos-com-A-valendo-11" será os pontos caso o "A" tenha o valor de "11".
    (if (> pontos-com-A-valendo-11 21) pontos-com-A-valendo-1 pontos-com-A-valendo-11))) ;Se a soma das cartas do usuário ultrapassar o "21", o "A" valerá "1", porém, se a soma das cartas do usuário não ultrapassar "21", o valor do "A" será "11".

;Funcionamento do "map":
;(map inc [2 3 4])
;O "map" aplicará a função em todos os elementos do vetor.

;Funcionamento do "reduce":
;(reduce + [2 3 4])
;(nil + 2) = 2
;(2 + 3) = 5
;(5 + 4) = 9

;Representação de um jogador:
;{
;   :player "Nome Teste" ;O nome de um jogador.
;   :cards [3, 4] ;As cartas de um jogador
;}

;O comando "def" define um símbolo de forma global, assim, ele será válido
;no namespace inteiro. Muitas vezes, não queremos que o escopo de um símbolo seja
;global, e sim, que ele possa ser acessado apenas de dentro de uma função, dessa
;forma, podemos utilizar o "let", que define um símbolo dentro do escopo da função "(let)".

;Basicamente, sempre que quisermos definir um símbolo dentro de uma função, devemos utilizar o "let".
(defn cria-jogador-e-define-cartas-iniciais ;Essa função criará as duas cartas, e, com essas duas cartas, criará um mapa que representará um jogador, esse mapa será retornado por essa função.
  "Define um jogador e gera as duas primeiras cartas iniciais para ele."
  [player-name]
  (let [card-1 (new-card) ;Estamos declarando os símbolos "card-1" e "card-2" com os seus respectivos valores, esses símbolos poderão ser acessados apenas dentro do escopo do "let".
        card-2 (new-card)
        cards [card-1 card-2]
        points (cards-points cards)]
  {:player-name player-name
   :cards [card-1 card-2]
   :points points}))


(card/print-player (cria-jogador-e-define-cartas-iniciais "Rafael"))