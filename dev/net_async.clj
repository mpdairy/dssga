(ns net-async
  (require [clojure.core.async :refer [<!! >!! close! <! >! go]]
           [net.async.tcp :refer :all]))

(defn echo-server []
  (let [acceptor (accept (event-loop) {:port 8345})]
    (loop []
      (when-let [server (<!! (:accept-chan acceptor))]
        (go
          (loop []
            (when-let [msg (<! (:read-chan server))]
              (when-not (keyword? msg)
                (do (prn msg)
                    (>! (:write-chan server) (.getBytes (str "ECHO/" (String. msg))))))
              (recur))))
        (recur)))))

(defn echo-client []
  (let [client (connect (event-loop) {:host "127.0.0.1" :port 8345})]
    (loop []
      (go (>! (:write-chan client) (.getBytes (str (rand-int 100000)))))
      (loop []
        (let [read (<!! (:read-chan client))]
          (when (and (keyword? read)
                     (not= :connected read))
            (recur))))
      (Thread/sleep (rand-int 3000))
      (recur))))

(comment
  (echo-server)
  (echo-client)
  )


