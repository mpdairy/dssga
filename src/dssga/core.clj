(ns dssga.core)


(defn make-population [num-indivs genes-per-indiv]
	(if (= 0 num-indivs)
		'()
		(cons {:fitness 0 :chromosome (vec (take num-indivs (repeatedly (fn [] (* 2 (- (* 2.0 (rand)) 1.0))))))}
			(population (dec num-indivs) genes-per-indiv))))


(defn get-individual [pop]
	(let [zeroindivs (filter (fn [a] (= (:fitness a) 0)) pop)
				total_fitness (reduce (fn [a b] (+ a (:fitness b))) 0 pop)]
		(letfn [(maybe-mutate [gene]
							(if (< (rand) (/ 1.0 80.0))
								(+ (* (if (< (rand) 0.5) -1 1) (expt (rand) 12)) gene)
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
