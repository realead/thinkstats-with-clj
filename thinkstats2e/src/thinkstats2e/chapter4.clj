(ns thinkstats2e.chapter4
  (:require [thinkstats2e.data :refer :all]
            [thinkstats2e.utils :refer :all]
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

(defn calc-cdf
    []
    (println (cdf [0 0 1 1 1 2 2 3]))
)

(defn test-eval-cdf
    []
    (let [c  (cdf [0 0 1 1 1 2 2 3])]
        (println "cdf(-100) = 0.0 :" (eval-cdf -100 c))
        (println "cdf(-1) = 0.0 :" (eval-cdf -1 c))
        (println "cdf(-0.5) = 0.0 :" (eval-cdf -0.5 c))
        (println "cdf(0) = 0.25 :" (eval-cdf 0 c))
        (println "cdf(0.5) = 0.25 :" (eval-cdf 0.5 c))
        (println "cdf(1) = 0.625 :" (eval-cdf 1.0 c))
        (println "cdf(1.5) = 0.625 :" (eval-cdf 1.5 c))
        (println "cdf(2) = 0.875 :" (eval-cdf 2.0 c))
        (println "cdf(2.5) = 0.875 :" (eval-cdf 2.5 c))
        (println "cdf(3) = 1 :" (eval-cdf 3 c))
        (println "cdf(200) = 1 :" (eval-cdf 200 c))
    )
)

(defn plot-cdf
   []
   (let [c (cdf [0 0 1 1 1 2 2 3])
         f (fn [x] (eval-cdf x c))]
        (-> (c/function-plot f -1 4)
            (i/view)
        )
   )
)

;; 4.4

(defn test-value-cdf
    []
    (let [c  (cdf [0 0 1 1 1 2 2 3])]
        (println "value(0.0) = -1 :" (value-cdf 0.0 c))
        (println "value(0.2) =  0 :" (value-cdf 0.2 c))
        (println "value(0.25) =  0 :" (value-cdf 0.25 c))
        (println "value(0.3) =  1 :" (value-cdf 0.3 c))
        (println "value(0.625) =  1 :" (value-cdf 0.625 c))
        (println "value(0.66) =  2 :" (value-cdf 0.66 c))
        (println "value(0.875) =  2 :" (value-cdf 0.875 c))
        (println "value(0.9) =  3 :" (value-cdf 0.9 c))
        (println "value(1) =  3 :" (value-cdf 1.0 c))
    )
)

(defn plot-prglngth
   [data]
   (let [lengths (->> data
                      (i/$where {:outcome 1})
                      (select-filtered :prglngth)
                 )
          c (cdf lengths)
          f (fn [x] (eval-cdf x c))
        ]
        (-> (c/function-plot f 0 60 
                   :x-label "weeks"
                   :y-label "cdf")
            (i/view)
        )
   )
)

;; 4.5
(defn create-cdf-f
  [series]
  (fn [x] (eval-cdf x (cdf series)))
)


(defn plot-weight-comparison
   [data]
   (let [first-children (->> data
                             (i/$where {:birthord 1 :prglngth {:gt 27}})
                             (select-filtered :totalwgt_kg)
                        )
         later-children (->> data 
                              (i/$where {:outcome 1 :birthord {:$ne 1} :prglngth {:gt 27}})
                              (select-filtered :totalwgt_kg)
                        )
         f-first (create-cdf-f first-children)
         f-second (create-cdf-f later-children)
        ]
        (-> (c/function-plot f-first 0 7
                   :x-label "weeks"
                   :y-label "cdf"
                   :series-label "cdf weight first children"
                   :legend true)
            (c/add-function f-second 0 7
                   :series-label "cdf weight second children")
            (i/view)
        )
   )
)

;; 4.6 

(defn weight-quantils
   [data]
   (let [first-children (->> data
                             (i/$where {:birthord 1 :prglngth {:gt 27}})
                             (select-filtered :totalwgt_kg)
                        )
         later-children (->> data 
                              (i/$where {:outcome 1 :birthord {:$ne 1} :prglngth {:gt 27}})
                              (select-filtered :totalwgt_kg)
                        )
         cdf-first (cdf first-children)
         cdf-later (cdf later-children)
        ]
        (println "first children:")
        (println  " median" (median-cdf cdf-first))
        (println  " iqr"  (iqr-cdf cdf-first))
        (println "later children:")
        (println  " median" (median-cdf cdf-later))
        (println  " iqr"  (iqr-cdf cdf-later))  
   )
)

;;4.7 

(defn show-percentil-cdf
   [data]
   (let [series    (->> data
                             (i/$where {:birthord 1 :prglngth {:gt 27}})
                             (select-filtered :totalwgt_kg)
                   )
         cdf-series (cdf series)
         samples (s/sample series :size 100)
         ranks (map #(percentil-rank-cdf % cdf-series) samples)
         f-ranks (create-cdf-f ranks)
        ]
        (-> (c/function-plot f-ranks 0 101
                   :x-label "percentil rank"
                   :y-label "cdf")
            (i/view)
        )
   )
)

(defn show-random
   [data]
   (let [series    (->> data
                             (i/$where {:birthord 1 :prglngth {:gt 27}})
                             (select-filtered :totalwgt_kg)
                   )
         cdf-series (cdf series)
         random-series (repeatedly 200 #(random-cdf cdf-series)) 
         f-first (create-cdf-f series)
         f-second (create-cdf-f random-series)
        ]
        (-> (c/function-plot f-first 0 7
                   :x-label "weeks"
                   :y-label "cdf"
                   :series-label "cdf data"
                   :legend true)
            (c/add-function f-second 0 7
                   :series-label "cdf random")
            (i/view)
        )
   )
)
