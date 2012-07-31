(use '[clojure.string :only (join)])

(defn card-name [card]
  (str (condp = (rank card)
         11 \J
         12 \Q
         13 \K
         1 \A
         (rank card))
       (suit card)))

(defn print-game [game player]
 (let [hand (nth (game :hands) player)
       player-number (+ player 1)]
   (println "Player" player-number)
   (println (join "  " (map card-name hand)))))

(defn valid-card-input? [input hand]
  (try
    (let [parsed (. Integer parseInt input)]
      (if (and (> parsed 0) (<= parsed (count hand)))
        parsed
        false))
    (catch NumberFormatException ex false)))

(defn get-card-input [game player msg]
  (let [hand (nth (game :hands) player)]
    (print-game game player)
    (println msg)
    (if-let [input (valid-card-input? (read-line) hand)]
      (- input 1)
      (do
        (println "Please enter a number between 1 and" (count hand))
        (println)
        (recur game player msg)))))

(defn add-cards-to-crib [game]
  (loop [game game turn 0]
    (if (< turn 4)
      (let [player (int (/ turn 2))
            msg (if (= (mod turn 2) 0)
                  "Select first crib card"
                  "Select second crib card")
            card-index (get-card-input game player msg)]
        (recur (add-to-crib game player card-index) (+ turn 1)))
      game)))

(defn game-loop []
  )
