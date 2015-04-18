(ns qttt.game
  "Logic relating to game state"
  (:require [clojure.set :as set]))

(def num-cells 9)
(def num-players 2)

(comment
  ;; Game Data Structures

  ;; Game
  {:turn   0
   :player 0
   :pair [4 2]
   :board board
   :base game
   }

  ;; The board
  {0 cell 1 cell}

   ;; A cell
  {:entanglements {0 {:player 0
                      :turn 0
                      :target 1
                      :focus false
                      :collapsing true}
                   1 {:player 1
                      :target 2
                      :turn 1
                      :focus true}}
   :classical {:player 0
               :turn 0
               :speculative true}})

(defn next-player
  [player]
  (mod (inc player) num-players))

(defn legal-spooky-mark?
  "Return true if a spooky mark can be placed on the given cell and subcell"
  [game cell subcell]
  (nil? (get-in game [:board cell :entanglements subcell])))

(defn check-collapses
  "Given a game, check if there are any collapses happening
   and return an updated game accordingly"
  [game]
  game)

(defn entangle
  "Given a cell, a subcell and the entangled cell and subcell, create an entanglement"
  [game cell subcell pair-cell pair-subcell]
  (if-not (legal-spooky-mark? game cell subcell)
    game
    (-> game
      ;; add the new spooky mark
      (assoc-in [:board cell :entanglements subcell]
        {:player (:player game)
         :turn (:turn game)
         :target pair-cell
         :focus false})
      ;; update the entangled spooky mark
      (update-in [:board pair-cell :entanglements pair-subcell] assoc
        :focus false
        :target cell)
      ;; remove the pair tracker
      (dissoc :pair)
      ;; change the player
      (update-in [:player] next-player)
      ;; update the turn
      (update-in [:turn] inc)
      ;; check for collapses
      (check-collapses))))

(defn resolve-collapse
  "Given a cell and subcell, resolve any collapse present in the game"
  [game cell subcell]
  (println "resolving collapse"))

(defn spooky-mark
  "Place a spooky mark at the given cell and subcell"
  [game cell subcell]
  (if-let [[pair-cell pair-subcell] (:pair game)]
    (entangle game cell subcell pair-cell pair-subcell)
    (if-not (legal-spooky-mark? game cell subcell)
      game
      (-> game
        (assoc :pair [cell subcell])
        (assoc-in  [:board cell :entanglements subcell]
          {:player (:player game)
           :turn (:turn game)
           :focus true})))))

(defn play
  "Play an action at the given cell and subcell.
   What the action is depends on the game context."
  [game cell subcell]
  (if (:collapsing game)
    (resolve-collapse game cell subcell)
    (spooky-mark game cell subcell)))

(defn speculate
  "Make a play, but store the previous game state so the 'play' can be easily reverted"
  [game cell subcell]
  (-> game
    (play cell subcell)
    (assoc :base game)))

(defn unspeculate
  "Restore the previous game state (if there was one)."
  [game]
  (if (:base game) (:base game) game))

(def new-game
  {:turn 0
   :player 0
   :board (zipmap (range num-cells)
                  (repeat {:entanglements {}}))})

