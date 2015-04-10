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
    (let [cells (set (map #(get-in game [:board %]) cids))
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

(defn legal-superposition?
  "Return true if the move is a legal superposition play. That is:

  - They must not be the same cell
  - They must not already be entangled
  - Neither cell must be classical"
  [game cid-1 cid-2]
  (let [c-1 (get-in game [:board cid-1])
        c-2 (get-in game [:board cid-2])]
    (not (or
           (= cid-1 cid-2)
           (:classical c-1)
           (:classical c-2)))))

(defn next-player
  [player]
  (mod (inc player) num-players))

(defn play
  "Given a board state and two cell ids, make a play. Mark the cells
   as entangled with a play by the current player, and return
   the new game state."
  [game cid-1 cid-2]
  (if (legal-superposition? game cid-1 cid-2)
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

(defn collapsing?
  "Return true if the given game has collapsing cells"
  [game]
  (some :collapsing (vals (:board game))))

(defn speculate-superposition
  "Speculate a play placing a superposition"
  [game cid-1 cid-2]
  (if (legal-superposition? game cid-1 cid-2)
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

(defn legal-collapse?
  "Return true if the move is a legal collapsing play. That is, the cell and the target cell
   are both collapsing."
  [game cid-1 cid-2]
  (let [c-1 (get-in game [:board cid-1])
        c-2 (get-in game [:board cid-2])]
    (and (:collapsing c-1) (:collapsing c-2))))

;; MAJOR PROBLEM: is my understanding of the game correct? Consider:

; A - B (o)
; B - C (x)
; C - A (o)
; A - D (x)

; Take observation [A D] (x). This implies, [B A] (o) [C A] (o). But this cannot be. B-C simply disappears in this
; scenario, although one of them must work.

;; One reference implemenation resolves the issue by only allowing you to resolve "within" the cycle. Ramifications
;; may proceed down branches, but you can only select from the original cycle.

;; Is this true? I *think* so.

;; when [A B] (o) -> [B C] (x) -> [C A] (o)
;;                -> [D A] (x)

;; Solution: read the original paper. See what it says.

;; Implications for me:

;; 1. Only vibrate the cycle (only nodes within the cycle are collapsing) (moderate). The only tricky part is not vibrating entanglements that point to outside the cycle.
;; 2. Only allow selection of nodes within the cycle for the resolution phase (easy).
;; 3. DO propagate effects of resolution to branches (easy).
;;
;; Should work! This will also prevent "evaporation" of branches outside the cycle, where a node may be rejected but
;; have no alternative for accepting.

(defn observe
  "Define an observation as a tuple of [accepted-cell-id rejected-cell-id], where the accepted cell is the cell that the
  value is observed collapsing 'into' and the rejected cell is the (formely) entangled cell that is being collapsed
  'out' of.

  Given an observation, return a set of observations implied based on the accepted cell's entanglements."
  [board [accepted-cid rejected-cid]]
  (println "observing...")
  (let [cell (board accepted-cid)
        entangled-cids (disj (get-entanglements cell) rejected-cid)
        _ (println "entangled: " entangled-cids)
        implied (set (map (fn [accept]
                            [accept accepted-cid])
                       entangled-cids))]
    implied))

(defn observe-all
  "Given a set of observations, recursively calculate *all* observations inferred from entangled cells"
  [board observations]
  (loop [os observations]
    (let [next-os (apply set/union os (map #(observe board %) os))]
      (if (= next-os os)
        next-os
        (recur next-os)))))

(defn speculate-collapse
  "Speculate a play resolving a collapse"
  [game cid-1 cid-2]
  (if (legal-collapse? game cid-1 cid-2)
    (if-let [spooky (get-in game [:board cid-1 :entanglements cid-2])]
      (do
        (println "collapse:" (observe-all (:board game) #{[cid-1 cid-2]}))
        (-> game
          (assoc-in [:board cid-1 :entanglements cid-2 :focus] true)
          #_(assoc-in [:board cid-2 :classical] {:player (:player spooky)
                                               :turn (:turn spooky)
                                               :speculative true
                                               :focus true})))
      game)
    game))

(defn speculate
  "Given a board state and two cells, return a new game
   state speculating a play to the given cells."
  [game cid-1 cid-2]
  (if (collapsing? game)
    (speculate-collapse game cid-1 cid-2)
    (speculate-superposition game cid-1 cid-2)))

(defn uninspect
  "Uninspect a single enganglement from an entanglement map"
  [entanglements cid]
  (if (get-in entanglements [cid :speculative])
    (dissoc entanglements cid)
    (if (get entanglements cid)
      (assoc-in entanglements [cid :focus] false)
      entanglements)))

(defn unspeculate-superposition
  "Remove speculative marks"
  [game cid-1 cid-2]
  (-> game
    (update-in [:board cid-1 :entanglements] #(uninspect % cid-2))
    (update-in [:board cid-2 :entanglements] #(uninspect % cid-1))))


(defn unspeculate-collapse
  "Remove speculative marks"
  [game cid-1 cid-2]
  (reduce (fn [game cell]
            (update-in game [:board cell] #(if (-> % :classical :speculative)
                                            (dissoc % :classical)
                                            %)))
    (-> game
      (update-in [:board cid-1 :entanglements] #(uninspect % cid-2))
      (update-in [:board cid-2 :entanglements] #(uninspect % cid-1)))
    (all-entanglements game #{cid-1})))

(defn unspeculate
  "Given a game and two cells, remove speculative entanglements
   and/or focus from both cells, returning a new game."
  [game cid-1 cid-2]
  (if (collapsing? game)
    (unspeculate-collapse game cid-1 cid-2)
    (unspeculate-superposition game cid-1 cid-2)))


(def new-game
  {:turn 0
   :player 0
   :board (zipmap (range num-cells)
                  (repeat {:entanglements {}}))})