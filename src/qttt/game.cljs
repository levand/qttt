(ns qttt.game
  "Logic relating to game state"
  (:require [clojure.set :as set]))

(def num-cells 9)
(def num-players 2)

(def ^:dynamic *speculative* false)

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
                      :pair [4 2]
                      :focus false
                      :collapsing true}
                   1 {:player 1
                      :pair [4 2]
                      :turn 1
                      :focus true}}
   :classical {:player 0
               :turn 0
               :speculative true}})

(defn next-player
  [player]
  (mod (inc player) num-players))

(defn get-entanglements
  "Return information regarding cells entangled with the given cell.

   Return value is a seq of [cell turn] tuples, where turn
   is the turn the spooky mark was placed. Exclude
   entanglements which have a classical value already."

  [game cell]
  (->> (get-in game [:board cell :entanglements])
    (vals)
    (keep (fn [e] (when-let [[pair-cell pair-subcell] (:pair e)]
                    (when-not (get-in game [:board pair-cell :classical])
                      [pair-cell (:turn e)]))))))

(defn index-of
  "Return the index of an item in a vector, if present. If not present return nil"
  [v item]
  (first (keep-indexed (fn [i val]
                         (when (= val item) i))
           v)))

(defn cycle-search
  "Return a seq of cell cycles discovered.

   Tracks the turn of the entanglement just
   followed, to avoid backtracking immediately."
  [game cell visited from-turn]
  (set (mapcat (fn [[edge turn]]
                 (when-not (= turn from-turn)
                   (if-let [idx (index-of visited edge)]
                     [(set (conj (subvec visited idx) cell))]
                     (cycle-search game edge (conj visited cell) turn))))
         (get-entanglements game cell))))

(defn detect-cycles
  "Return a sequence of all entanglement cycles present in the given board"
  [game]
  ;; Not 100% efficient, but we need some way of detecting multiple disjoint graphs.
  ;; Should be fine for small N. Using sets removes redundancies. If we run into perf
  ;; trouble we can track which nodes we've visited *at all* and never revisit them.
  (apply set/union (map #(cycle-search game % [] -1) (keys (:board game)))))

(defn check-collapses
  "Given a game, check if there are any collapses happening
   and return an updated game accordingly"
  [game]
  (let [cycles (detect-cycles game)]
    (if (empty? cycles)
      (assoc game :collapsing false)
      (let [collapsing-cells (apply set/union cycles)]
        (reduce (fn [g cell]
                  (let [collapsing-subcells
                        (keep
                          (fn [[sub e]] (when (contains? collapsing-cells (first (:pair e))) sub))
                          (get-in g [:board cell :entanglements]))]
                    (reduce #(assoc-in %1 [:board cell :entanglements %2 :collapsing] true) g collapsing-subcells)))
          (assoc game :collapsing true)
          collapsing-cells)))))

(defn legal-spooky-mark?
  "Return true if a spooky mark can be placed on the given cell and subcell"
  [game cell subcell]
  (and
    (not= cell (first (:pair game)))
    (nil? (get-in game [:board cell :entanglements subcell]))))

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
         :pair [pair-cell pair-subcell]
         :focus *speculative*})
      ;; update the entangled spooky mark
      (update-in [:board pair-cell :entanglements pair-subcell] assoc
        :focus *speculative*
        :pair [cell subcell])
      ;; remove the pair tracker
      (dissoc :pair)
      ;; change the player
      (update-in [:player] next-player)
      ;; update the turn
      (update-in [:turn] inc)
      ;; check for collapses
      (check-collapses))))

(defn valid-collapse?
  "Return true if the given cell and subcell represent
   a valid choice to collapse"
  [game cell subcell]
  (get-in game [:board cell :entanglements subcell :collapsing]))


;; will this work with the new data model? Or need refactoring?
(defn observe
  "Define an observation as a tuple of [accepted-cell accepted-subcell],
   where the accepted subcell is that which will be observed and become classical.

  Given an observation, return a set of observations implied based
  on the accepted cell's entanglements."
  [game [accepted-cell accepted-subcell]]
  (set (map :pair
         (vals (dissoc (get-in game [:board accepted-cell :entanglements])
                 accepted-subcell)))))

(defn observe-all
  "Given a set of observations, recursively calculate *all* observations inferred from entangled cells"
  [game observations]
  (loop [os observations]
    (let [next-os (apply set/union os (map #(observe game %) os))]
      (if (= next-os os)
        next-os
        (recur next-os)))))

(defn resolve-collapse
  "Given a cell and subcell, resolve any collapse present in the game"
  [game cell subcell]
  (if-not (valid-collapse? game cell subcell)
    game
    (let [observations (observe-all game #{[cell subcell]})
          observations (if *speculative* (disj observations [cell subcell])
                                         observations)]
      (reduce (fn [game [cell subcell]]
                (let [e (get-in game [:board cell :entanglements subcell])]
                  (assoc-in game [:board cell :classical]
                    {:player (:player e)
                     :turn (:turn e)
                     :focus *speculative*})))
        (if *speculative*
          (assoc-in game [:board cell :entanglements subcell :focus] true)
          (assoc game :collapsing false))
        observations))))

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

(defn highlight
  "Focus the subcell and its entanglement"
  [game cell subcell]
  (-> (if-let [[pair-cell pair-subcell]
               (get-in game [:board cell :entanglements subcell :pair])]
        (assoc-in game [:board pair-cell :entanglements pair-subcell :focus] true)
        game)
    (assoc-in [:board cell :entanglements subcell :focus] true)))

(defn play
  "Play an action at the given cell and subcell.
   What the action is depends on the game context."
  [game cell subcell]
  (if (:collapsing game)
    (resolve-collapse game cell subcell)
    (if (and *speculative* (get-in game [:board cell :entanglements subcell]))
      (highlight game cell subcell)
      (spooky-mark game cell subcell))))

(defn speculate
  "Make a play, but store the previous game state so the 'play' can be easily reverted"
  [game cell subcell]
  (binding [*speculative* true]
    (-> game
      (play cell subcell)
      (assoc :base game))))

(defn unspeculate
  "Restore the previous game state (if there was one)."
  [game]
  (if (:base game) (:base game) game))

(def new-game
  {:turn 0
   :player 0
   :board (zipmap (range num-cells)
                  (repeat {:entanglements {}}))})

