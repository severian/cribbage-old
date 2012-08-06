(use '[clojure.string :only (join)])

(defn card-name [card]
  (str (case (rank card)
         11 \J
         12 \Q
         13 \K
         1 \A
         (rank card))
       (suit card)))

(defn card-str [cards]
  (join "  " (map card-name cards)))

(defn score-name [score]
  (str
    (score-val score)
    " points for "
    (score-desc score)
    (if (score-cards score)
      (str
        " ("
        (card-str (score-cards score))
        ")"))))

(defn score-str [scores]
  (if (> (count scores) 0)
    (join ", " (map score-name scores))))

(defn print-game [game player]
  (let [hand (get-unplayed-hand game player)
        scores (score-str (game :score-details))]
    (println "Player 1's Score:" (get-score game 0))
    (if (and scores (= player 1))
      (println scores))
    (println "Player 2's Score:" (get-score game 1))
    (if (and scores (= player 2))
      (println scores))
    (if (> (count (game :play-cards)) 0)
      (do 
        (println "Played Cards:" (card-str (reverse (game :play-cards))))
        (println (str (played-points game) "pts"))))
    (println)
    (println "Player" (str (+ player 1) "'s turn"))
    (if (game :error)
      (println (game :error)))
    (println (card-str hand))))

(defn valid-card-input? [input hand]
  (try
    (let [parsed (. Integer parseInt input)]
      (if (and (> parsed 0) (<= parsed (count hand)))
        parsed
        false))
    (catch NumberFormatException ex false)))

(defn get-card-input [game player msg]
  (let [hand (get-unplayed-hand game player)]
    (print-game game player)
    (println msg)
    (if-let [input (valid-card-input? (read-line) hand)]
      (- input 1)
      (do
        (println "Please enter a number between 1 and" (count hand))
        (println)
        (recur game player msg)))))

(defn play-cards [game]
  (if (< (played-card-count game) 8)
    (let [player (game :player)
          card-index (get-card-input game player "Play a card")]
      (recur (play-card game player card-index)))
    [game nil]))

(defn add-cards-to-crib [game]
  (loop [game game turn 0]
    (if (< turn 4)
      (let [player (int (/ turn 2))
            msg (if (= (mod turn 2) 0)
                  "Select first crib card"
                  "Select second crib card")
            card-index (get-card-input game player msg)]
        (recur (add-to-crib game player card-index) (+ turn 1)))
      [game play-cards])))

(defn game-loop []
  (loop [[game game-fn] [(make-game) add-cards-to-crib]]
    (if (not (nil? game-fn))
      (recur (game-fn game)))))
