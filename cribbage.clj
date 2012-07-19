(defn make-deck []
  (for [suit (seq "CSHD")
        card (map vector
                [1 2 3 4 5 6 7 8 9 10 11 12 13]
                [1 2 3 4 5 6 7 8 9 10 10 10 10])]
      (into [suit] card)))

(defn suit [card]
  (nth card 0))

(defn rank [card]
  (nth card 1))

(defn value [card]
  (nth card 2))

(defn sort-hand [hand]
  (sort-by rank hand))

(defn other-player [player]
  (if (= player 0) 1 0))

(defn remove-from-hand [hand card-index]
  (concat (subvec hand 0 card-index)
          (subvec hand (+ card-index 1) (count hand))))

(defn sum-values [cards]
  (reduce + (map value cards)))

(defn add-score [game player score]
  (let [current-score (nth (game :scores) player)
        new-score (+ current-score score)
        new-scores (assoc (game :scores) player new-score)]
    (assoc game :scores new-scores)))

(defn same-rank-length [cards]
  (loop [first-card (first cards)
         remainder (rest cards)
         acc 1]
    (if (and (not (empty? remainder))
             (= (rank first-card) (rank (first remainder))))
      (recur (first remainder) (rest remainder) (+ acc 1))
      acc)))

(defn score-pairs [cards]
  (condp = (same-rank-length cards)
    2 2
    3 6
    4 12
    0))

(defn is-run? [cards]
  (let [sorted (sort-hand cards)]
    (loop [first-card (first sorted)
           remainder (rest sorted)]
      (if (empty? remainder)
        true
        (let [next-card (first remainder)]
          (if (= -1 (- (rank first-card) (rank next-card)))
            (recur next-card (rest remainder))
            false))))))

(defn score-run [cards]
  (cond (< (count cards) 3) 0
        (is-run? cards) (count cards)
        :else (recur (rest cards))))

(defn can-play? [game player]
  (let [remaining (- 31 (sum-values (game :play-cards)))
        played-cards (nth (game :played-cards) player)]
    (loop [hand (nth (game :played-cards) player)
           index 0]
      (cond (empty? hand) false
            (and (not (contains? played-cards index))
                 (<= (value (first (hand))) remaining)) true
            :else (recur (rest hand) (+ index 1))))))

(defn add-to-crib [game player card-index]
  (let [hand (nth (game :hands) player)]
    (if (> (count hand) 4)
      (let [card (nth hand card-index)
            new-hand (remove-from-hand hand card-index)
            new-hands (assoc (game :hands) player new-hand)
            crib (conj (game :crib) card)]
        (assoc game :hands new-hands :crib crib))
      game)))

(defn score-play [game player]
  (let [play-cards (game :play-cards)
        values (sum-values play-cards)
        can-play (can-play? (other-player player))]
    (+ (score-run play-cards)
       (score-pairs play-cards)
       (if (= values 15) 2 0)
       (if (= values 31) 1 0)
       (if (not can-play) 1 0))))
       
(defn play-card [game player card-index]
  (let [played-cards (nth (game :played-cards) player)]
    (if (not (contains? played-cards card-index))
      (let [hand (nth (game :hands) player)
            card (nth hand card-index)
            player-played-cards (conj played-cards card-index)
            new-played-cards (assoc played-cards player player-played-cards)
            new-play-cards (conj (game :play-cards) card)]
        (assoc game
               :played-cards new-played-cards
               :play-cards new-play-cards))
      game)))

(defn make-game []
  (let [deck (shuffle (make-deck))]
    { :hands [(sort-hand (subvec deck 0 6))
              (sort-hand (subvec deck 6 12))]
      :played-cards [#{}, #{}]
      :scores [0 0]
      :play-cards '()
      :crib []
      :starter (nth deck 12) } ))

(defn score-starter [game]
  (if (= (rank (game :starter)) 11)
    (add-score game 0 2)
    game))

