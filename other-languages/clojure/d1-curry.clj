#!/usr/bin/env clj
(let
  [
    data (map read-string (clojure.string/split-lines (slurp "../../day1-inputs.txt")))
  ]
  [
   ; Part 1
   (defn tot [f] (fn [xs] (reduce + (map f xs))))
   (def func #(- (quot % 3) 2))

   ; Part 2
   (defn func-2 [x] (let
                      [res (func x)]
                      (if (>= (func x) 0) (+ res (func-2 res)) 0)
                      ))

   (println ((tot func) data))
   (println ((tot func-2) data))
  ]
)
