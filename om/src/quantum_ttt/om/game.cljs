(ns quantum-ttt.om.game
  "Logic relating to game state"
  (:require [clojure.set :as set]))

(def num-cells 9)
(def num-players 2)

(comment
  ;; Game Data Structures

  ;; Game
  {:turn   0
   :player 0
   :board  board}

  ;; The board
  {0 cell
   1 cell}

   ;; A cell
  {:entanglements {0 {:player 0
                      :turn 0
                      :focus false
                      :speculative false}
                   1 {:player 1
                      :turn 1
                      :focus true
                      :speculative true}}
   :classical {:player 0
               :turn 0
               :speculative true}
   :collapsing false})

(defn index-of
  "Return the index of an item in a vector, if present. If not present return nil"
  [v item]
  (first (keep-indexed (fn [i val]
                         (when (= val item) i))
           v)))

(defn get-entanglements
  "Gets the entanglement ids of a cell, disregarding speculative ones"
  [cell]
  (set (map (fn [[k v]]
              (when-not (:speculative v)
                k))
         (:entanglements cell))))

(defn all-entanglements
  "Returns a set of all cell ids entangled with any of the given cell ids.

  Includes the given IDs."
  [game cell-ids]
  (loop [cids cell-ids]
    (let [cells (set (map #(get-in game [:board %]) cell-ids))
          neighbors (apply set/union (map get-entanglements cells))]
      (if (= cids neighbors)
        neighbors
        (recur neighbors)))))

(defn cycle-search
  "Generic depth-first search, tracking visited nodes.

   Return a seq of cycles discovered."
  [node edges visited prev]
  (set (mapcat (fn [edge]
                 (if-let [idx (index-of visited edge)]
                   [(set (conj (subvec visited idx) node))]
                   (cycle-search edge edges (conj visited node) node)))
         (disj (set (edges node)) prev))))

(defn detect-cycles
  "Return a sequence of all entanglement cycles present in the given board"
  [board]
  (let [edges (fn [cell-id]
                (get-entanglements (board cell-id)))]
    ;; Not 100% efficient, but we need some way of detecting multiple disjoint graphs.
    ;; Should be fine for small N. Using sets removes redundancies. If we run into perf
    ;; trouble we can track which nodes we've visited *at all* and never revisit them.
    (apply set/union (map #(cycle-search % edges [] nil) (keys board)))))

(defn check-collapses
  "Given a game, if there are any cycles, mark all cells involved in a cycle,
   and all entangled cells, as collapsing."
  [game]
  (let [cycles (detect-cycles (:board game))]
    (if (empty? cycles)
      game
      (reduce (fn [g cell]
                (assoc-in g [:board cell :collapsing] true))
        game (all-entanglements game (apply set/union cycles))))))

(defn legal-play?
  "Return true if the move is legal. That is:

  - This or any other cells may not be collapsing (that needs to be resolved first)
  - They must not be the same cell
  - They must not already be entangled
  - Neither cell must be classical"
  [game cid-1 cid-2]
  (let [c-1 (get-in game [:board cid-1])
        c-2 (get-in game [:board cid-2])]
    (not (or
           (= cid-1 cid-2)
           (:classical c-1)
           (:classical c-2)
           (some :collapsing (vals (:board game)))))))

(defn next-player
  [player]
  (mod (inc player) num-players))

(defn play
  "Given a board state and two cell ids, mark the cells
   as entangled with a play by the current player, and return
   the new game state."
  [game cid-1 cid-2]
  (if (legal-play? game cid-1 cid-2)
    (-> game
      (update-in [:board cid-1 :entanglements cid-2] assoc
        :player (:player game)
        :speculative false
        :turn (:turn game))
      (update-in [:board cid-2 :entanglements cid-1] assoc
        :player (:player game)
        :speculative false
        :turn (:turn game))
      (update-in [:player] next-player)
      (update-in [:turn] inc)
      (check-collapses))
    game))

(defn speculate
  "Given a board state and two cells, return a new game state
   reflecting the fact that the user is speculating the superposition
   between those cells"
  [game cid-1 cid-2]
  (if (legal-play? game cid-1 cid-2)
    (let [inspect* (fn [entanglements cid]
                     (if (get entanglements cid)
                       (assoc-in entanglements [cid :focus] true)
                       (assoc entanglements cid {:player (:player game)
                                                 :turn (:turn game)
                                                 :focus true
                                                 :speculative true})))]
      (-> game
        (update-in [:board cid-1 :entanglements] #(inspect* % cid-2))
        (update-in [:board cid-2 :entanglements] #(inspect* % cid-1))))
    game))

(defn unspeculate
  "Given a game and two cells, remove speculative entanglements
   and/or focus from both cells, returning a new game."
  [game cid-1 cid-2]
  (let [uninspect* (fn [entanglements cid]
                     (if (get-in entanglements [cid :speculative])
                       (dissoc entanglements cid)
                       (if (get entanglements cid)
                         (assoc-in entanglements [cid :focus] false)
                         entanglements)))]
    (-> game
          (update-in [:board cid-1 :entanglements] #(uninspect* % cid-2))
          (update-in [:board cid-2 :entanglements] #(uninspect* % cid-1)))))




(def new-game
  {:turn 0
   :player 0
   :board (zipmap (range num-cells)
                  (repeat {:entanglements {}}))})