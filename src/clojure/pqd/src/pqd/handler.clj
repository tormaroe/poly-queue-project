(ns pqd.handler
  (:require [compojure.core :refer :all]
            [compojure.handler :as handler]
            [clj-json.core :as json]))

(def queues (ref {}))

(defn add-queue-if-needed [queues name]
  (if (contains? queues name)
      queues
      (assoc queues name {:name name, :head []})))

(defn get-queue [name]
  (let [queues (alter queues add-queue-if-needed name)]
    (get queues name)))

(defn set-queue [q new-q]
  (ref-set queues (assoc @queues (:name q) new-q)))

(defn queue-push [q value]
  ;; Checking for (nil? value) not needed, will never happen 
  (let [new-head (conj (:head q) value)
        new-q (assoc q :head new-head)]
    (set-queue q new-q)
    new-q))

(defn queue-pop [q]
  (let [[head & tail] (:head q)
        new-q (assoc q :head (vec tail))]
    (set-queue q new-q)
    [head new-q]))

(defn json-response [data]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defn queue-response [q & extras]
  (->> (list "queue" (:name q)
             "count" (count (:head q)))
       (concat extras)
       (apply hash-map)
       json-response))

(defroutes app-routes
  (context "/:queue-name" [queue-name]
    
    (GET "/" []
        (let [[value queue] (dosync 
                              (-> queue-name 
                                  get-queue 
                                  queue-pop))]
          (queue-response queue 
                          "value" value
                          "eof" (nil? value))))
        
    (GET "/count" [] 
      (dosync (-> queue-name 
                  get-queue 
                  queue-response)))
    
    (POST "/" {body :body}
      (let [data (slurp body)]
        (dosync (-> queue-name get-queue
                    (queue-push data)
                    queue-response))))))

(def app (handler/api app-routes))