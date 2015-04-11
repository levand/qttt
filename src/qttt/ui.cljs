(ns qttt.ui
  (:require [goog.Uri :as uri]
            [qttt.ui.om :as om]
            [qttt.ui.reagent :as reagent]))

(enable-console-print!)

(defn ^:export main
  []
  (let [lib (keyword (.get (.getQueryData (goog.Uri. js/location)) "lib"))]
    (case lib
      :om (om/main)
      :reagent (reagent/main)
      (js/alert "Please use the query string to set 'lib' to om, reagent or quiescent."))))


