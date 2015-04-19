(ns qttt.ui.om
  (:require [om.core :as om]
            [om.dom :as d]
            [qttt.game :as game]))

(def css-transition-group (js/React.createFactory js/React.addons.CSSTransitionGroup))

(defn class-name
  "Return a class name string given multiple CSS classes. Nil classes are filtered out."
  [& classes]
  (apply str (interpose " " (filter identity classes))))

(defn mark
  "Return a Om DOM node for a player's mark"
  [{:keys [player turn focus collapsing]}]
  (let [icon (if (= 0 player) "fa-plus" "fa-circle-o")
        player-class (if (= 0 player) "player-x" "player-o")]
    (d/span #js {:key (str player)
                   :className (class-name "mark" player-class (when focus "highlight")
                                (when collapsing "shake") (when collapsing "shake-constant"))}
      (d/span #js {:className (class-name "fa" icon)})
      (d/span #js {:className "turn"} turn))))

(defn entanglement
  "Om component for an individual entanglement"
  [e owner]
  (let [[_ cell _ subcell] (om/path e)
        game-cursor (om/root-cursor (om/state e))]
    (reify
      om/IRender
      (render [this]
        (d/td #js {:className (class-name (if (empty? e) "empty-mark" "spooky-mark"))
                     :onClick (fn [evt]
                                (om/transact! game-cursor
                                  #(game/play (game/unspeculate %) cell subcell)))
                     :onMouseEnter (fn [evt]
                                     (om/transact! game-cursor
                                       #(game/speculate % cell subcell)))
                     :onMouseLeave (fn [evt]
                                     (om/transact! game-cursor game/unspeculate))}
          (css-transition-group #js {:transitionName "mark-transition"}
            (when-not (empty? e) (mark e))))))))

(defn superposition
  "Om component for a quantum cell"
  [cell owner]
  (reify
    om/IRender
    (render [this]
      (d/td #js {:className (class-name "superposition")}
        (apply d/table nil
          (map (fn [row]
                 (apply d/tr nil
                   (map (fn [idx]
                          ;; Make sure were have a valid cursor
                          (let [e (get-in cell [:entanglements idx]
                                    (get-in (assoc-in cell [:entanglements idx] {})
                                      [:entanglements idx]))]
                            (om/build entanglement e)))
                     row)))
            (partition 3 (range 9))))))))

(defn classical
  "Om component for a classical cell"
  [cell owner]
  (reify
    om/IRender
    (render [this]
      (d/td #js {:className "classical"}
        (mark (:classical cell))))))

(defn cell
  "Om component for a square"
  [cell owner]
  (if (:classical cell)
    (classical cell owner)
    (superposition cell owner)))

(defn instructions [game owner]
  (reify
    om/IRender
    (render [this]
      (let [[player phase] (game/instructions game)
            player-classes (if (zero? player)
                             ["player-x" "fa-plus"]
                             ["player-o" "fa-circle-o"])]
        (d/div #js {:className "instructions"}
          (d/span #js {:className (apply class-name "mark" "fa" player-classes)})
          (str "'s turn: " phase))))))

(defn board [game owner]
  (reify
    om/IRender
    (render [this]
      (d/div #js {:className "board-container"}
        (om/build instructions game)
        (apply d/table #js {:className "board"}
          (map (fn [row]
                 (apply d/tr nil
                   (map (fn [idx]
                          (om/build cell
                            (get-in game [:board idx]))) row)))
            (partition 3 (range 9))))
        (d/div #js {:className "repo-link"}
          (d/a #js {:href "http://github.com/levand/qttt"}
            "http://github.com/levand/qttt"))))))

(defn screen [game owner]
  (reify
      om/IRender
      (render [this]
        (d/div #js {:className "play-area"}
          (om/build board game)))))


(defn ^:export main
  []
  (let [game-state (atom game/new-game)]
    (om/root
      screen
      game-state
      {:target (. js/document (getElementById "root"))})))
