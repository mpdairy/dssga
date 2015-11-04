(ns scratch
  (:require
   [dssga.ga :as ga]))

(take 10 (ga/make-population 30 10))

(def poplist
  (->> (ga/make-population 10 4)
       (map (fn [c] (let [fitness (rand-int 100)]
                     [fitness {:fitness fitness
                               :chrome c}])))
       (sort-by #(* -1 (first %)))))

(ga/roulette-select-worst 1 1 poplist)

(ga/mutate-gene 1.0)

