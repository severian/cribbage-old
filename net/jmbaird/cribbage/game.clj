(defn make-deck []
  (for [suit (seq "\u2660\u2665\u2666\u2663")
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
  ((fn rm [cards acc]
    (cond (empty? cards) '()
          (= acc card-index)
            (rm (rest cards) (+ acc 1))
          :else
            (conj (rm (rest cards) (+ acc 1)) (first cards))))
     hand 0))

(defn get-hand [game player]
  (nth (game :hands) player))

(defn get-unplayed-hand [game player]
  (nth (game :unplayed-hands) player))

(defn get-score [game player]
  (nth (game :scores) player))

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

(defn can-play? [game play-cards player]
  (let [remaining (- 31 (sum-values play-cards))
        hand (get-unplayed-hand game player)]
    (boolean (some #(<= (rank %) remaining) hand))))

(defn add-to-crib [game player card-index]
  (let [hand (nth (game :hands) player)]
    (if (> (count hand) 4)
      (let [card (nth hand card-index)
            new-hand (remove-from-hand hand card-index)
            new-hands (assoc (game :hands) player new-hand)
            new-crib (concat (game :crib) card)]
        (assoc game :hands new-hands :unplayed-hands new-hands :crib new-crib))
      game)))

(defn score-play [game play-cards can-play player]
  (let [values (sum-values play-cards)]
    (+ (score-run play-cards)
       (score-pairs play-cards)
       (if (= values 15) 2 0)
       (if (= values 31) 1 0)
       (if (not can-play) 1 0))))
       
(defn play-card [game player card-index]
  (let [hand (get-unplayed-hand game player)
        new-hand (remove-from-hand hand card-index)
        new-hands (assoc (game :unplayed-hands) player new-hand)
        new-play-cards (conj (game :play-cards) (nth hand card-index))
        can-play (can-play? game new-play-cards (other-player player)) 
        new-score (+ (get-score game player)
                     (score-play game new-play-cards can-play player))
        new-scores (assoc (game :scores) player new-score)]
    (assoc game
           :scores new-scores
           :unplayed-hands new-hands
           :play-cards (if can-play new-play-cards '()))))

(defn make-game []
  (let [deck (shuffle (make-deck))
        hands [(sort-hand (subvec deck 0 6))
               (sort-hand (subvec deck 6 12))]]
    { :hands hands
      :unplayed-hands hands
      :scores [0 0]
      :play-cards '()
      :crib []
      :starter (nth deck 12) } ))

(defn score-starter [game]
  (if (= (rank (game :starter)) 11)
    (add-score game 0 2)
    game))

