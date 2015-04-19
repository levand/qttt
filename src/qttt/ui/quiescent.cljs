(ns qttt.ui.quiescent
  (:require [quiescent.core :as q]
            [quiescent.dom :as d]
            [contextual.core :as c]
            [qttt.game :as game]))

(defn class-name
  "Return a class name string given multiple CSS classes. Nil classes are filtered out."
  [& classes]
  (apply str (interpose " " (filter identity classes))))

(defn mark
  [{:keys [player turn focus collapsing]}]
  (let [icon (if (= 0 player) "fa-plus" "fa-circle-o")
        player-class (if (= 0 player) "player-x" "player-o")]
    (d/span {:className (class-name "mark" player-class
                                    (when focus "highlight")
                                    (when collapsing "shake")
                                    (when collapsing "shake-constant"))}
            (d/span {:className (class-name "fa" icon)})
            (d/span {:className "turn"} turn))))

(q/defcomponent Entanglement
  [e game-atom]
  (let [[_ cell _ subcell] (c/context e)]
    (d/td {:className (if (empty? e) "empty-mark" "spooky-mark")
           :onClick (fn [evt]
                      (swap! game-atom #(game/play (game/unspeculate %) cell subcell)))
           :onMouseEnter (fn [evt]
                           (swap! game-atom #(game/speculate % cell subcell)))
           :onMouseLeave (fn [evt]
                           (swap! game-atom #(game/unspeculate %)))}
          (q/CSSTransitionGroup {:transitionName "mark-transition"}
                                (when-not (empty? e) (mark e))))))

(defn superposition
  [cell game-atom]
  (d/td {:className "superposition"}
        (apply d/table {}
               (for [row (partition 3 (range 9))]
                 (apply d/tr {}
                        (for [idx row]
                          (let [e (get-in cell [:entanglements idx]
                                          (c/contextualize {} (concat (c/context cell)
                                                                      [:entanglements idx])))]
                            (Entanglement e game-atom))))))))


(defn classical
  [cell game-atom]
  (d/td {:className "classical"} (mark (:classical cell))))

(q/defcomponent Cell
  [cell game-atom]
  (if (:classical cell)
    (classical cell game-atom)
    (superposition cell game-atom)))

(q/defcomponent Board
  [cells game-atom]
  (d/div {:className "board-container"}
         (apply d/table {:className "board"}
                (for [row (partition 3 (range 9))]
                  (apply d/tr {}
                         (for [idx row] (Cell (get cells idx) game-atom)))))
    (d/div {:className "repo-link"}
          (d/a {:href "http://github.com/levand/qttt"} "http://github.com/levand/qttt"))))

(q/defcomponent Screen
  [game game-atom]
  (d/div {:className "play-area"}
    (Board (:board (c/contextualize game)) game-atom)))

(defn render-loop
  "Render the main application. Called every frame."
  [game-state]
  (q/render (Screen @game-state game-state)
            (.getElementById js/document "root"))
  (.requestAnimationFrame js/window #(render-loop game-state)))

(defn ^:export main
  []
  (let [game-state (atom game/new-game)]
    (render-loop game-state)))
