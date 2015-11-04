(ns dssga.ga
  (:require
   [clojure.math.numeric-tower :as math]))

(defn make-gene []
  (* 2 (- (* 2.0 (rand)) 1.0)))

(defn make-chromosome [genes-per-chromosome]
  (take genes-per-chromosome (repeatedly make-gene)))

(defn make-population [num-indivs genes-per-chromosome]
  (take num-indivs (repeatedly (partial make-chromosome genes-per-chromosome))))

;; Takes [[fitness1 v] [fitness2 v] ...]
(defn roulette-select
  ([fitness-pairs] (roulette-select 1 fitness-pairs))
  ([selection-pressure fitness-pairs]
     (let [vsum    (reduce + (map #(math/expt % selection-pressure) (map first fitness-pairs)))
           stop-at (* (rand) vsum)]
       (loop [pairs    fitness-pairs
              counting 0]
         (let [[fitness entity] (first pairs)
               fitness     (math/expt fitness selection-pressure)]
           (if (> (+ counting fitness) stop-at)
             entity
             (recur (rest pairs) (+ counting fitness))))))))

(defn roulette-select-worst
  ([fitness-pairs] (roulette-select-worst 1 0.00001 fitness-pairs))
  ([selection-pressure lowest-fitness fitness-pairs]
     (roulette-select
      selection-pressure
      (map (fn [[fitness entity]] [(/ 1.0 (max lowest-fitness fitness)) entity])
           fitness-pairs))))

(defn crossover [& chromosomes]
  (if (empty? chromosomes)
    []
    (let [non-empty (remove empty? chromosomes)]
      (if (not= (count non-empty) (count chromosomes))
        (apply crossover non-empty)
        (cons (first (rand-nth chromosomes))
              (apply crossover (map rest chromosomes)))))))

(defn mutate-gene
  ([gene] (mutate-gene 1 gene))
  ([severity gene]
     (if (= 0 severity) gene
         (+ (* (rand-nth [-1 1]) (math/expt (rand) (/ 15 severity))) gene))))

(defn mutate-chromosome
  ([chromosome] (mutate-chromosome (/ 1 80) 1 chromosome))
  ([chance-of-mutation-per-gene severity chromosome]
     (map (fn [gene]
            (if (< chance-of-mutation-per-gene (rand))
              (mutate-gene severity gene)
              gene)))))


;; the two functions below are from the first go I had at programming
;; with Clojure and and they still need to be split up to use the
;; above functions...

(defn get-individual [pop]
  (let [zeroindivs (filter (fn [a] (= (:fitness a) 0)) pop)
        total_fitness (reduce (fn [a b] (+ a (:fitness b))) 0 pop)]
    (letfn [(maybe-mutate [gene]
              (if (< (rand) (/ 1.0 80.0))
                (+ (* (if (< (rand) 0.5) -1 1) (math/expt (rand) 12)) gene)
                gene))
            (roulette [p stopfit]
              (if (<= (- stopfit (:fitness (first p))) 0)
                (first p)
                (roulette (rest p) (- stopfit (:fitness (first p))))))
            (breed [indiv1 indiv2]
              (letfn [(crossover [chrome1 chrome2]
                        (if (empty? chrome1)
                          '()
                                        ;(cons (maybe-mutate (if (< (rand) 0.5) (first chrome1) (first chrome2))) (crossover (rest chrome1) (rest chrome2)))
                          (cons (maybe-mutate (/ (+ (first chrome1) (first chrome2)) 2)) (crossover (rest chrome1) (rest chrome2)))
                          ))]
                {:fitness 0 :chromosome (vec (crossover (:chromosome indiv1) (:chromosome indiv2)))}))]
      (if (empty? zeroindivs)
        (breed (roulette pop (* (rand) total_fitness)) (roulette pop (* (rand) total_fitness)))
        (first zeroindivs)))))


(defn put-individual [indiv pop]
  (let [zeroindivs (filter (fn [a] (= (:fitness a) 0)) pop)]
    (if (empty? zeroindivs)
      (let [fitnesses (map (fn [a] (:fitness a)) pop)
            max_fitness (reduce (fn [a b] (if (< a b) b a)) fitnesses)
            invert_fitnesses (map (fn [a] (- 1.0 (/ a max_fitness))) fitnesses)
            total_invert_fitness (reduce + invert_fitnesses)]
        (letfn [(roulette-worst [p fitlist stopfit]
                  (if (<= (- stopfit (first fitlist)) 0)
                    (cons indiv (rest p))
                    (cons (first p) (roulette-worst (rest p) (rest fitlist) (- stopfit (first fitlist))))))]
          (roulette-worst pop invert_fitnesses (* (rand) total_invert_fitness))))
      (letfn [(replacezero [p]
                (if (= 0 (:fitness (first p)))
                  (cons indiv (rest p))
                  (cons (first p) (replacezero (rest p)))))]
        (replacezero pop)))))
