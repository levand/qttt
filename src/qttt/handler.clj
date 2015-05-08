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
  (route/not-found "<h1>Page not found</h1>"))

(def app (wrap-dir-index #'site-routes))
