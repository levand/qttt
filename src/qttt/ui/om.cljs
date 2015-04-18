(ns qttt.ui.om
  (:require [om.core :as om]
            [om.dom :as dom]
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
    (dom/span #js {:key (str player)
                   :className (class-name "mark" player-class (when focus "highlight")
                                (when collapsing "shake") (when collapsing "shake-constant"))}
      (dom/span #js {:className (class-name "fa" icon)})
      (dom/span #js {:className "turn"} turn))))

(defn entanglement
  "Om component for an individual entanglement"
  [e owner]
  (let [[_ cid-1 _ cid-2] (om/path e)
        game-cursor (om/root-cursor (om/state e))]
    (reify
      om/IRender
      (render [this]
        (dom/td #js {:className (class-name (if (empty? e) "empty-mark" "spooky-mark"))
                     :onClick (fn [evt]
                                #_(om/transact! game-cursor
                                  #(game/play % cid-1 cid-2)))
                     :onMouseEnter (fn [evt]
                                     #_(om/transact! game-cursor
                                       #(game/speculate % cid-1 cid-2)))
                     :onMouseLeave (fn [evt]
                                     #_(om/transact! game-cursor
                                       #(game/unspeculate % cid-1 cid-2)))}
          (css-transition-group #js {:transitionName "mark-transition"}
            (when-not (empty? e) (mark e))))))))

(defn superposition
  "Om component for a quantum cell"
  [cell owner]
  (reify
    om/IRender
    (render [this]
      (dom/td #js {:className (class-name "superposition")}
        (apply dom/table nil
          (map (fn [row]
                 (apply dom/tr nil
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
      (dom/td #js {:className "classical"}
        (mark (:classical cell))))))

(defn cell
  "Om component for a square"
  [cell owner]
  (if (:classical cell)
    (classical cell owner)
    (superposition cell owner)))

(defn board [cells owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "board-container"}
        (apply dom/table #js {:className "board"}
          (map (fn [row]
                 (apply dom/tr nil
                   (map (fn [idx]
                          (om/build cell
                            (get cells idx))) row)))
            (partition 3 (range 9))))))))

(defn screen [game owner]
  (reify
      om/IRender
      (render [this]
        (dom/div #js {:className "play-area"}
          (om/build board (:board game))))))


(defn ^:export main
  []
  (let [game-state (atom game/new-game)]
    (om/root
      screen
      game-state
      {:target (. js/document (getElementById "root"))})))
