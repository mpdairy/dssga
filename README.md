# dssga

A Distributable Steady State Genetic Algorithm.  There are three functions:

```clj
(make-population num-indivs genes-per-indiv)  ;--> new random population

(get-individual pop)  ;--> new untested individual

(put-individual indiv pop)  ;--> latest population
```

Individuals are maps {:fitness 0 :chromosome [0.3224 -0.9324 0.43214 0.001 -0.43224 ...]}

Genes are random between -1.0 and 1.0.

Populations are lists of individuals.

get-individual breeds a new individual from two adults using roulette wheel selection, or any untested individuals (with fitness 0)

put-individual takes an individual with fitness > 0 and returns a population with the new indiv added and one other dropped out.  The killed individual is based on inverted roulette wheel selection, where lower fitness has higher chance of being selected.

## Installation ##

Just add the dssga dependency to your project.clj file:
```clj
(defproject my-project "1.0.0"
:dependencies [[org.clojure/clojure "1.5.1"]
[dssga "0.1.1"]])
```

## Usage Examples ##

### Max Ones ###

Evolve a chromosome with max positive genes

```clj
(use 'dssga.core)

(defn max-ones [pop individuals_tested]
(loop [pop pop individuals_tested individuals_tested]
(if (= 0 individuals_tested)
(reduce (fn [a b] (if (> (:fitness a) (:fitness b)) a b)) {:fitness 0} pop)
(letfn [(calc-fitness [indiv]
(max 0.1 (* 1.0 (reduce (fn [a b] (+ a b)) 0 (:chromosome indiv)))))]
(let [indiv (get-individual pop)
fitness (calc-fitness indiv)]
(recur (put-individual (assoc indiv :fitness fitness) pop) (dec individuals_tested)))))))

;evolves population of 100 individuals, 10 genes each, for 10000 individuals (100 generations)
(max-ones (make-population 100 10) 10000)
```

## Upcoming Changes ##

We're going to add dssga-server and dssga-client functions that use TCP/IP so that the evolution can be distributed across a network.

## License

Copyright © 2014 Matt Parker

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
