(ns quantum-ttt.om.game
  "Logic relating to game state")

(comment
  ;; Game Data Structures

  ;; A mark
  {:type :spooky      ; or :classical
   :player :x         ; owner of the mark
   :focus true        ; Visually highlight the mark
                      ; hasn't been actually placed yet
   :turn 4            ; Turn when the mark was placed. Nil or absent
                      ; if the mark is speculative (has not actually been played).
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
                          (assoc-in [:board square cell] {:type :spooky
                                                          :player (:player game)
                                                          :focus true})
                          (assoc-in [:board cell square]  {:type :spooky
                                                           :player (:player game)
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
  (if (nil? (get-in game [:board square cell :turn]))
    ;; It was a transient mark
    (-> game
      (assoc-in [:board square cell] {})
      (assoc-in [:board cell square] {}))
    ;; It was a focused existing mark
    (-> game
      (assoc-in [:board square cell :focus] false)
      (assoc-in [:board cell square :focus] false))))

(defn legal-move?
  "Return true if the move is legal"
  [game square cell]
  (let [v (get-in game [:board square cell])]
    (and (not= square cell) (not (:turn v)))))

(defn other-player
  [player]
  (if (= :x player) :o :x))


(defn play-spooky
  "Given a board state, a square and a cell, mark the cell
   for the current player and return the new game state"
  [game square cell]
  (if (legal-move? game square cell)
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
      (update-in [:turn] inc))
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

