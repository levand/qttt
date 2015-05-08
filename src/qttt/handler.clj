(ns qttt.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            ))

(defn wrap-dir-index [handler]
  (fn [req]
    (handler
     (update-in req [:uri]
                #(if (= "/" %) "/dev.html" %)))))


(defroutes site-routes
  (route/resources "/")
  (POST "/game" []
        ;; take an empty POST
        ;; return a new game, including game ID
        )
  (POST "/move" []
        
        ;; take new move as EDN in POST
        
        ;; apply move to current game state
        
        ;; return new game state
        
       )
  (GET "/state" []
       )
  (route/not-found "<h1>Page not found</h1>"))

(def app (wrap-dir-index #'site-routes))
