(ns thinkstats2e.chapter4
  (:require [thinkstats2e.data :refer :all]
            [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as s]
  )
)


;; 4.2


(defn percentile-rank
  [scores score]
  (let  [n (count scores)
         m (->> scores
                (filter #(<= % score))
                (count)
           )
        ]
        (->(* 100 (/ m n))
           (double)
        )
   )             
)


(defn percentile 
   [scores rank]
   (->> scores
        (sort)
        (drop-while #(< (percentile-rank scores %) rank))
        (first)
   )
)


(defn percentile2
   [scores rank]
   (let [index  (int (* (/ rank 100)
                        (dec (count scores))
                     )
                )
        ]
        (nth (sort scores) index)
   )
)

(defn check-percentiles
  []
  (let [scores [4 5 6 33 2 44 90 11 34 5 55 66 10]
        vals (map #(* 10 %) (range 11))
        ranks (map #(percentile-rank scores %) vals)
        per1 (map #(percentile scores %) ranks)
        per2 (map #(percentile2 scores %) ranks)
       ]
       (println "scores:" (sort scores))
       (println "vals: " vals)
       (println "ranks:" ranks)
       (println "per1: " per1) 
       (println "per2: " per2)
  )           
)

;; 4.3


