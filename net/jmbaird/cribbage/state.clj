(ns net.jmbaird.cribbage.state
  (:require [net.jmbaird.cribbage.game :as game]))

(defn make-state [state-name game player]
  { :state-name state-name
    :game game
    :player player })

(defn swap-player [state]
  (assoc state :player (game/other-player (state :player))))

(defn crib-transition [state]
  (let [game (state :game)]
    (if (some #(> (count %) 4) (game :hands))
      state
      (make-state :play game 1))))

(defn play-transition
  (let [game (state :game)]
    (if (some #(<= (count % 4) (game :played-cards)))
      (make-state :play game (state :other-player)
      (make-state :play game (state :player))))))

(def state-fns
  { :crib { :play-fn game/add-to-crib
            :transition-fn crib-transition }
    :play { :play-fn game/play-card
            :transition-fn play-transition }})

(defn state-transition [state]
  (let [fns (state-fns (state :state-name))]
    ))

