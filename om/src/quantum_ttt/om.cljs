(ns quantum-ttt.om
  (:require [om.core :as om]
            [om.dom :as dom]
            [quantum-ttt.om.game :as game]))

(enable-console-print!)

(def css-transition-group (js/React.createFactory js/React.addons.CSSTransitionGroup))

(defn class-name
  "Return a class name string given multiple CSS classes. Nil classes are filtered out."
  [& classes]
  (apply str (interpose " " (filter identity classes))))

(defn mark
  "Return a Om DOM node for a player's mark"
  [{:keys [player turn focus]}]
  (let [icon (if (= :x player) "fa-plus" "fa-circle-o")
        player-class (if (= :x player) "player-x" "player-o")]
    (dom/span #js {:key (str (name player))
                   :className (class-name "mark" "fa" icon player-class (when focus "highlight"))})))

(defn cell
  "Om component for a cell which may be either empty, or a spooky mark"
  [m owner]
  (let [[cell square] (take 2 (reverse (om/path m)))
        game-cursor (om/root-cursor (om/state m))]
    (reify
      om/IRender
      (render [this]
        (println "rendering..." (empty? m))
        (dom/td #js {:className (if (empty? m) "empty-mark" "spooky-mark")
                     :onClick (fn [evt]
                                (om/transact! game-cursor
                                  #(game/play-spooky % square cell)))
                     :onMouseEnter (fn [evt]
                                     (om/transact! game-cursor
                                       #(game/inspect % square cell)))
                     :onMouseLeave (fn [evt]
                                     (om/transact! game-cursor
                                       #(game/uninspect % square cell)))}
          (css-transition-group #js {:className "ctg" :transitionName "mark-transition"}
            (when-not (empty? m) (mark m))))))))

(defn superposition
  "Om component for a quantum marked square"
  [sq owner]
  (reify
    om/IRender
    (render [this]
      (dom/td #js {:className "superposition"}
        (apply dom/table nil
          (map (fn [row]
                 (apply dom/tr nil
                   (map (fn [m]
                          (om/build cell m))
                     row)))
            (partition 3 sq)))))))

(defn classical
  "Om component for a classically marked square"
  [sq owner]
  (reify
    om/IRender
    (render [this]
      (dom/td #js {:className "classical"}
        (mark sq)))))

(defn square
  "Om component for a square"
  [sq owner]
  (if (= :classical (:type sq))
    (classical sq owner)
    (superposition sq owner)))

(defn board [squares owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "board-container"}
        (apply dom/table #js {:className "board"}
                  (map (fn [row]
                         (apply dom/tr nil
                           (map #(om/build square %) row)))
                    (partition 3 squares)))))))

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