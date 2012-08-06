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

(defn make-score [score cards desc]
  [score cards desc])

(defn score-val [score]
  (nth score 0))

(defn score-cards [score]
  (nth score 1))

(defn score-desc [score]
  (nth score 2))

(defn score-total [scores]
  (reduce + (map score-val scores)))

(defn remove-from-hand [hand card-index]
  ((fn rm [cards acc]
    (cond
      (empty? cards)
        '()
      (= acc card-index)
        (rm (rest cards) (+ acc 1))
      :else
        (conj (rm (rest cards) (+ acc 1)) (first cards))))
     hand 0))

(defn get-hand [game player]
  (nth (game :hands) player))

(defn get-unplayed-hand [game player]
  (nth (game :unplayed-hands) player))

(defn get-card [game player card-index]
  (nth (get-unplayed-hand game player) card-index))

(defn get-score [game player]
  (nth (game :scores) player))

(defn sum-values [cards]
  (reduce + (map value cards)))

(defn played-points [game]
  (sum-values (game :play-cards)))

(defn can-play? [game play-cards]
  (let [remaining (- 31 (sum-values play-cards))
        hand (get-unplayed-hand game (other-player (game :player)))]
    (boolean (some #(<= (rank %) remaining) hand))))

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

(defn score-pair [game cards]
  (case (same-rank-length cards)
    2 (make-score 2 (take cards 2) "a pair")
    3 (make-score 6 (take cards 3) "a pair royal")
    4 (make-score 12 (take cards 4) "a double pair royal")
    nil))

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

(defn score-run [game cards]
  (cond
    (< (count cards) 3)
      nil
    (is-run? cards)
      (make-score (count cards) cards (str "a run of " (count cards)))
    :else
      (recur game (drop-last cards))))

(defn score-15 [game cards]
  (if (= (sum-values cards) 15)
    (make-score 2 nil "fifteen")))

(defn score-last [game cards]
  (cond
    (= (sum-values cards) 31)
      (make-score 2 nil "thirty-one")
    (not (can-play? game cards))
      (make-score 1 nil "last")))

(def *score-fns*
  [score-pair score-run score-15 score-last])

(defn score-play [game cards]
  (loop [score-fns *score-fns* scores '()]
    (if (empty? score-fns)
      scores
      (let [score ((first score-fns) game cards)]
        (recur
          (rest score-fns)
          (if (nil? score)
            scores
            (conj scores score)))))))

(defn add-to-crib [game player card-index]
  (let [hand (nth (game :hands) player)]
    (if (> (count hand) 4)
      (let [card (nth hand card-index)
            new-hand (remove-from-hand hand card-index)
            new-hands (assoc (game :hands) player new-hand)
            new-crib (concat (game :crib) card)]
        (assoc game
               :hands new-hands
               :unplayed-hands new-hands
               :crib new-crib
               :error nil))
      game)))

(defn played-card-count [game]
  (- 8 (reduce + (map count (game :unplayed-hands)))))

(defn can-play-card? [game card-index]
  (let [remaining (- 31 (sum-values (game :play-cards)))
        player (game :player)
        card (get-card game player card-index)]
    (<= (value card) remaining)))

(defn play-card [game player card-index]
  (cond
    (not= player (game :player))
      (assoc game :error "Not your turn!")
    (not (can-play-card? game card-index))
      (assoc game :error "Can't play that card.")
    :else
      (let [hand (get-unplayed-hand game player)
            new-hand (remove-from-hand hand card-index)
            new-hands (assoc (game :unplayed-hands) player new-hand)
            new-play-cards (conj (game :play-cards) (nth hand card-index))
            can-play (can-play? game new-play-cards)
            score-details (score-play game new-play-cards)
            new-score (+ (get-score game player)
                         (score-total score-details))
            new-scores (assoc (game :scores) player new-score)]
        (assoc game
               :scores new-scores
               :score-details score-details
               :unplayed-hands new-hands
               :play-cards (if can-play new-play-cards '())
               :player (other-player player)
               :error nil))))

(defn make-game []
  (let [deck (shuffle (make-deck))
        hands [(sort-hand (subvec deck 0 6))
               (sort-hand (subvec deck 6 12))]]
    { :hands hands
      :unplayed-hands hands
      :player 1
      :scores [0 0]
      :score-details '()
      :play-cards '()
      :crib []
      :starter (nth deck 12) } ))

(defn score-starter [game]
  (if (= (rank (game :starter)) 11)
    (add-score game 0 2)
    game))

