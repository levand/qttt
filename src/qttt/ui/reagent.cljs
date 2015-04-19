(ns qttt.ui.reagent
  (:require [reagent.core :as r]
            [qttt.game :as game]))

(def game (r/atom game/new-game))

(def css-transition-group (js/React.createFactory js/React.addons.CSSTransitionGroup))

(defn class-name
  "Return a class name string given multiple CSS classes. Nil classes are filtered out."
  [& classes]
  (apply str (interpose " " (filter identity classes))))

(defn mark
  [{:keys [player turn focus collapsing]}]
  (let [icon (if (= 0 player) "fa-plus" "fa-circle-o")
        player-class (if (= 0 player) "player-x" "player-o")]
    [:span {:class (class-name "mark" player-class
                               (when focus "highlight")
                               (when collapsing "shake")
                               (when collapsing "shake-constant"))}
     [:span {:class (class-name "fa" icon)}]
     [:span.turn turn]]))

(defn entanglement
  [e cell subcell]
  [:td {:class (if (empty? e) "empty-mark" "spooky-mark")
        :on-click (fn [evt]
                    (swap! game #(game/play (game/unspeculate %) cell subcell)))
        :on-mouse-enter (fn [evt]
                          (swap! game #(game/speculate % cell subcell)))
        :on-mouse-leave (fn [evt]
                          (swap! game game/unspeculate))}
   (css-transition-group #js {:transitionName "mark-transition"}
       (when-not (empty? e) (r/as-element (mark e))))])

(defn superposition
  [cell cell-idx]
  [:td.superposition
   [:table (for [row (partition 3 (range 9))]
             ^{:key (str row)}
             [:tr (for [idx row]
                    (let [e (get-in cell [:entanglements idx] {})]
                      ^{:key idx}
                      [entanglement e cell-idx idx]))])]])

(defn classical
  [cell cell-idx]
  [:td.classical (mark (:classical cell))])

(defn cell
  [cell cell-idx]
  (if (:classical cell)
    ^{:key cell-idx} [classical cell cell-idx]
    ^{:key cell-idx} [superposition cell cell-idx]))

(defn board [cells]
  [:div.board-container
   [:table.board (for [row (partition 3 (range 9))]
                   ^{:key (str row)}
                   [:tr (for [idx row]
                          (cell (get cells idx) idx))])]
   [:div.repo-link
    [:a {:href "http://github.com/levand/qttt"} "http://github.com/levand/qttt"]]])

(defn screen []
  [:div.play-area [board (:board @game)]])

(defn ^:export main
  []
  (r/render
   [screen]
   (. js/document (getElementById "root"))))
