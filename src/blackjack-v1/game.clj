(ns blackjack-v1.game
  (:require [card-ascii-art.core :as card]))

; A = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, J = 11, Q = 12, K = 13
; 1...13
(defn new-card []
  "Gera uma carta entre 1 e 13"
  (inc (rand-int 13)))

; calcula os pontos de acordo com as cartas
; J, Q, K = 10 (nao 11, 12 e 13)
; A = 11 porem, se passar de 21, A = 1
; Regra => cartas com valor > 10 recebem 10
(defn JQK->10 [card]
  (if (> card 10) 10 card))

(defn A->11 [card]
  (if (= card 1) 11 card))

(defn points-cards [cards]
  (let [cards-without-JQK (map JQK->10 cards)
        cards-with-A11 (map A->11 cards-without-JQK)
        points-with-A-1 (reduce + cards-without-JQK)
        points-with-A-11 (reduce + cards-with-A11)]
    (if (> points-with-A-11 21) points-with-A-1 points-with-A-11)))

; como representar um jogador
; {:player "Bruno Ferraz"
;  :cards [3 4]}
(defn player [player-name]
  (let [card-1 (new-card)
        card-2 (new-card)
        cards [card-1 card-2]
        points (points-cards cards)]
    {:player-name player-name
     :cards       cards
     :points      points}))

; chamando a funcao new-card para gerar uma nova carta
; atualizar o vetor card dentro do player com a nova carta
; calcular os pontos do jogador com o novo vetor de cartas
; retornar esse novo jogador
(defn more-card [player]
  (let [card (new-card)
        cards (conj (:cards player) card)
        new-player (update player :cards conj card)
        points (points-cards cards)]
    (assoc new-player :points points)))

(defn player-decision-continue? [player]
  (= (read-line) "sim"))

(defn dealer-decision-continue? [player-points dealer]
  (let [dealer-points (:points dealer)]
    (< dealer-points player-points)))

; funcao game, responsavel por perguntar ao jogador se ele quer
; mais cartas, caso queira, chamar a funcao more-card
(defn game [player fn-decision-continue?]
  (println (:player-name player) ": mais uma carta?")
  (if (fn-decision-continue? player)
    (let [player-with-more-cards (more-card player)]
      (card/print-player player-with-more-cards)
      (recur player-with-more-cards fn-decision-continue?))
    player))

(def jogador (player "Bruno Ferraz"))
(card/print-player jogador)

(def dealer (player "Dealer"))
(card/print-player dealer)

(def player-after-game (game jogador player-decision-continue?))
(game dealer (partial dealer-decision-continue? (:points player-after-game)))

; se ambos passar de 21, ambos perdem
; se os pontos forem iguais, empatam
; se player passou de 21, dealer ganha
; se dealer passou de 21, player ganha
; se player > dealer, player ganha
; se dealer > player, dealer ganha
(defn end-game [player dealer]
  (let [player-points (:points player)
        dealer-points (:points dealer)
        player-name (:player-name player)
        dealer-name (:player-name dealer)
        message (cond (and (> player-points 21) (> dealer-points 21)) "Ambos perderam!"
                      (= player-points dealer-points) "Empatou!"
                      (> player-points 21) (str dealer-name " ganhou!")
                      (> dealer-points 21) (str player-name " ganhou!")
                      (> dealer-points player-points) (str dealer-name " ganhou!")
                      (> player-points dealer-points) (str player-name " ganhou!"))]
    (card/print-player player)
    (card/print-player dealer)
    (print message)))
