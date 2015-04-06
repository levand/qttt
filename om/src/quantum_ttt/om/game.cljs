(ns quantum-ttt.om.game
  "Logic relating to game state"
  (:require [clojure.set :as set]))

(comment
  ;; Game Data Structures

  ;; A mark
  {:type :spooky      ; or :classical, or :transient
   :player :x         ; owner of the mark
   :focus true        ; Visually highlight the mark
                      ; hasn't been actually placed yet
   :turn 4            ; Turn when the mark was placed.
   }

  ;; A game
  {:turn 0            ; the current turn
   :player :x         ; who plays next
   :collapse?  false  ; true iff the player must resolve a state collapse
                      ; before playing a normal turn.
   :board  [...]      ; vector of 9 squares
   }

  ;; A square = a vector of 9 (empty mark|spooky mark), or one classical mark
  )

(defn inspect
  "Given a board state, a square and a cell idx, return a new game state
   reflecting the fact that the user is inspecting that square."
  [game square cell]
  (let [contents (get-in game [:board square cell])]
    (cond
      ;; No-op, can't select same square
      (= square cell) game
      ;; Make two transient spooky marks
      (empty? contents) (-> game
                          (assoc-in [:board square cell] {:type :transient
                                                          :player (:player game)
                                                          :turn (:turn game)
                                                          :focus true})
                          (assoc-in [:board cell square]  {:type :transient
                                                           :player (:player game)
                                                           :turn (:turn game)
                                                           :focus true}))
      ;; Highlight the existing mark at that location
      :else (-> game
              (assoc-in [:board square cell :focus] true)
              (assoc-in [:board cell square :focus] true)))))

(defn uninspect
  "Given a board state, a square and a cell idx, remove ransient
   marks and/or focus from the cell and its entangled pair,
   returning a new game state"
  [game square cell]
  (if (= :transient (get-in game [:board square cell :type] :transient))
    (-> game
      (assoc-in [:board square cell] {})
      (assoc-in [:board cell square] {}))
    (-> game
      (assoc-in [:board square cell :focus] false)
      (assoc-in [:board cell square :focus] false))))

(defn index-of
  "Return the index of an item in a vector, if present. If not present return nil"
  [v item]
  (first (keep-indexed (fn [i val]
                         (when (= val item) i))
           v)))

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
  (let [edges (fn [node]
                (filter #(= :spooky (get-in board [node % :type]))
                  (range (count board))))]
    ;; Not 100% efficient, but we need some way of detecting multiple disjoint graphs.
    ;; Should be fine for small N. Using sets removes redundancies. If we run into perf
    ;; trouble we can track which nodes we've visited *at all* and never revisit them.
    (apply set/union (map #(cycle-search % edges [] nil)
                       (range (count board))))))

(defn maybe-collapse
  "Given a game, if there are any cycles, replace them with
   collapsing squares and switch the player accordingly,
   returning the new game."
  [game]
  (let [cycles (detect-cycles (:board game))]
    (if (empty? cycles)
      game
      (do
        (println "FOUND CYCLES: " (str cycles))
        game))))

(defn legal-mark?
  "Return true if the move is legal"
  [game square cell]
  (let [v (get-in game [:board square cell])]
    (and (not= square cell) (or (empty? v)
                              (= :transient (:type v))))))

(defn other-player
  [player]
  (if (= :x player) :o :x))

(defn play-spooky
  "Given a board state, a square and a cell, mark the cell
   for the current player and return the new game state"
  [game square cell]
  (if (legal-mark? game square cell)
    (-> game
      (update-in [:board square cell] assoc
        :type :spooky
        :player (:player game)
        :turn (:turn game))
      (update-in [:board cell square] assoc
        :type :spooky
        :player (:player game)
        :turn (:turn game))
      (update-in [:player] other-player)
      (update-in [:turn] inc)
      (maybe-collapse))
    game))


(def empty-superposition
  (vec (repeat 9 {})))

(def new-game
  {:turn 0
   :player :x
   :board (vec (repeat 9 empty-superposition))})




(comment

  (defn sample-superposition []
    (vec (repeatedly 9
           (fn []
             (if (zero? (rand-int 4))
               {:player (if (zero? (rand-int 2)) :x :o)
                :placed (rand-int 9)}
               nil)))))

  (defn sample-square []
    (if (zero? (rand-int 3))
      {:player (if (zero? (rand-int 2)) :x :o)
       :placed (rand-int 9)}
      (sample-superposition)))

  (defn sample-board []
    (vec (repeatedly 9 sample-square)))

  (defn sample-game []
    {:turn 0
     :board (sample-board)}))

