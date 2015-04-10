(ns qttt.game
  "Logic relating to game state"
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTANT NOTE

; This data model is not a full implementation. It does not support one important feature - the capacity to create a
; cycle between just two cells using two different entanglements. This data model supports only one entanglement slot
; between two cells.


; The UI examples are still good, the game just isn't fully playable right now. I will fix it, but it might not be
; done in time for Clojure/West, particularly as a lot of this could be cleaned up a great deal by some judicious use
; of core.logic as well.

(def num-cells 9)
(def num-players 2)

(comment
  ;; Game Data Structures

  ;; Game
  {:turn   0
   :player 0
   :board  board
   }

  ;; The board
  {0 cell
   1 cell}

   ;; A cell
  {:entanglements {0 {:player 0
                      :turn 0
                      :focus false
                      :speculative false
                      :collapsing true}
                   1 {:player 1
                      :turn 1
                      :focus true
                      :speculative true}}
   :classical {:player 0
               :turn 0
               :speculative true}})

(defn index-of
  "Return the index of an item in a vector, if present. If not present return nil"
  [v item]
  (first (keep-indexed (fn [i val]
                         (when (= val item) i))
           v)))

(defn get-entanglements
  "Gets the entanglement ids from a cell ID. Disregards speculative entanglements, or defunct entanglements to a
  (non-speculatively) classical cell."
  [game cell-id]
  (let [cell (get-in game [:board cell-id])]
    (set (keep (fn [[target-cell-id entanglement]]
                 (when-not (:speculative entanglement)
                   (let [target-cell (get-in game [:board target-cell-id])]
                     (when-not (and (:classical target-cell)
                                 (not (-> target-cell :classical :speculative)))
                       target-cell-id))))
           (:entanglements cell)))))

(defn all-entanglements
  "Returns a set of all cell ids entangled with any of the given cell ids.

  Includes the given IDs."
  [game cell-ids]
  (loop [cids cell-ids]
    (let [neighbors (apply set/union cids (map #(get-entanglements game %) cids))]
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
  [game]
  (let [edges (fn [cell-id]
                (get-entanglements game cell-id))]
    ;; Not 100% efficient, but we need some way of detecting multiple disjoint graphs.
    ;; Should be fine for small N. Using sets removes redundancies. If we run into perf
    ;; trouble we can track which nodes we've visited *at all* and never revisit them.
    (apply set/union (map #(cycle-search % edges [] nil) (keys (:board game))))))


(defn check-collapses
  "Given a game, if there are any cycles, all involved entanglements as collapsing."
  [game]
  (let [cycles (detect-cycles game)]
    (if (empty? cycles)
      (assoc game :collapsing false)
      (let [collapsing-cells (apply set/union cycles)]
        (reduce (fn [g cell-id]
                  (let [to-collapse (set/intersection collapsing-cells
                                      (set (get-entanglements g cell-id)))]
                    (reduce #(assoc-in %1 [:board cell-id :entanglements %2 :collapsing] true)
                      g to-collapse)))
          (assoc game :collapsing true)
          collapsing-cells)))))

(defn legal-superposition?
  "Return true if the move is a legal superposition play (or speculation of a play). That is:

      - They must not be the same cell
      - They must not already be entangled
      - Neither cell must be classical"
  [game cid-1 cid-2 speculative? ]
  (let [c-1 (get-in game [:board cid-1])
        c-2 (get-in game [:board cid-2])]
    (not (or
           (= cid-1 cid-2)
           (:classical c-1)
           (:classical c-2)
           (and (not speculative?)
             (contains? (get-entanglements game cid-1) cid-2))))))

(defn next-player
  [player]
  (mod (inc player) num-players))

(defn legal-collapse?
  "Return true if the move is a legal collapsing play. That is, is the selected entanglement collapsing?"
  [game cid-1 cid-2]
  (get-in game [:board cid-1 :entanglements cid-2 :collapsing]))

(defn observe
  "Define an observation as a tuple of [accepted-cell-id rejected-cell-id], where the accepted cell is the cell that the
  value is observed collapsing 'into' and the rejected cell is the (formely) entangled cell that is being collapsed
  'out' of.

  Given an observation, return a set of observations implied based on the accepted cell's entanglements."
  [game [accepted-cid rejected-cid]]
  (let [entangled-cids (disj (get-entanglements game accepted-cid) rejected-cid)
        implied (set (map (fn [accept]
                            [accept accepted-cid])
                       entangled-cids))]
    implied))

(defn observe-all
  "Given a set of observations, recursively calculate *all* observations inferred from entangled cells"
  [game observations]
  (loop [os observations]
    (let [next-os (apply set/union os (map #(observe game %) os))]
      (if (= next-os os)
        next-os
        (recur next-os)))))

(defn collapse
  "Resolve a play choosing a collapse"
  [game cid-1 cid-2 speculate?]
  (if (legal-collapse? game cid-1 cid-2)
    (let [observations (observe-all game #{[cid-1 cid-2]})
          observations (if speculate? (disj observations [cid-1 cid-2]) observations)]
      (reduce (fn [game [accepted-cid rejected-cid]]
                (let [entanglement (get-in game [:board accepted-cid :entanglements rejected-cid])]
                  (assoc-in game [:board accepted-cid :classical]
                    {:player (:player entanglement)
                     :turn (:turn entanglement)
                     :speculative speculate?
                     :focus speculate?})))
        (if speculate?
          (assoc-in game [:board cid-1 :entanglements cid-2 :focus] true)
          (assoc game :collapsing false))
        observations))
    game))

(defn superposition
  "Make a play placing a superposition"
  [game cid-1 cid-2]
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
    (check-collapses)))

(defn play
  "Given a board state and two cell ids, make a play."
  [game cid-1 cid-2]
  (if (:collapsing game)
    (if (legal-collapse? game cid-1 cid-2)
      (collapse game cid-1 cid-2 false)
      game)
    (if (legal-superposition? game cid-1 cid-2 false)
      (superposition game cid-1 cid-2)
      game)))

(defn speculate-superposition
  "Speculate a play placing a superposition"
  [game cid-1 cid-2]
  (if (legal-superposition? game cid-1 cid-2 true)
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


(defn speculate
  "Given a board state and two cells, return a new game
   state speculating a play to the given cells."
  [game cid-1 cid-2]
  (if (:collapsing game)
    (collapse game cid-1 cid-2 true)
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
  (reduce (fn [game cell-id]
            (update-in game [:board cell-id] #(if (-> % :classical :speculative)
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
  (if (:collapsing game)
    (unspeculate-collapse game cid-1 cid-2)
    (unspeculate-superposition game cid-1 cid-2)))


(def new-game
  {:turn 0
   :player 0
   :board (zipmap (range num-cells)
                  (repeat {:entanglements {}}))})
