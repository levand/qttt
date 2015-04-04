(ns quantum-ttt.om
  (:require [om.core :as om]
            [om.dom :as dom]))

(enable-console-print!)

(defn empty-qbit [_ owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "empty-qbit"}))))


(defn played-qbit [[player turn] owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "qbit"}
        (dom/span #js {:className "player"} (name player))
        #_(dom/span #js {:className "turn"} turn)))))


(defn qbit [play owner]
  (if (nil? play)
    (empty-qbit play owner)
    (played-qbit play owner)))

(defn class-name
  "Return a class name string given multiple CSS classes"
  [& classes]
  (apply str (interpose " " classes)))

(defn mark
  "Return a Om DOM node for a player's mark"
  [{:keys [player turn]}]
  (let [icon (if (= :x player) "fa-plus" "fa-circle-o")
        player-class (if (= :x player) "player-x" "player-o")]
    (dom/span #js {:className (class-name "mark" "fa" icon player-class)})))

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
                   (map (fn [spooky-mark]
                          (if spooky-mark
                            (dom/td #js {:className "spooky-mark"} (mark spooky-mark))
                            (dom/td #js {:className "empty-mark"})))
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
  (if (sequential? sq)
    (superposition sq owner)
    (classical sq owner)))

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


(defn sample-superposition []
  (repeatedly 9
    (fn []
      (if (zero? (rand-int 4))
        {:player (if (zero? (rand-int 2)) :x :o)
         :turn (rand-int 9)}
        nil))))

(defn sample-square []
  (if (zero? (rand-int 3))
    {:player (if (zero? (rand-int 2)) :x :o)
     :turn (rand-int 9)}
    (sample-superposition)))


(defn sample-board []
  (repeatedly 9 sample-square))

(defn ^:export main
  []
  (let [app-state (atom {:turn 0
                         :board (sample-board)})]
    (om/root
      screen
      app-state
      {:target (. js/document (getElementById "root"))})))