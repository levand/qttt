(ns qttt.server
  (:require [qttt.game :as game])
  (:import [java.util UUID]))

(def games
  "An atom of map of game. A poor-man's game database."
  (atom {})) 

(defn new-key []
  (str (UUID/randomUUID)))

(defn new-game []
  (let [key (new-key)
        game game/new-game]
    (swap! games #(assoc % key (atom game)))
    {:key key :game game}))

(defn apply-move [game-id move]
  
  )

(defn get-game []

  )
