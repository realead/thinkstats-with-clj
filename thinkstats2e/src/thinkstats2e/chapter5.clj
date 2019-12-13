(ns thinkstats2e.chapter5
  (:require [thinkstats2e.data :refer :all]
            [thinkstats2e.utils :refer :all]
            [incanter.core :as i]
            [incanter.io :as iio]
            [incanter.charts :as c]
            [incanter.stats :as s]
  )
)


;; 5.1

(defn create-exp-cdf
  [alpha]
  (fn [x] (- 1.0 (i/exp (- (* alpha x)))))
)


(defn show-exp-cdfs
  []
  (-> (c/function-plot (create-exp-cdf 0.5) 0 3
           :x-label "x"
           :y-label "cdf"
           :series-label "lambda=0.5"
           :legend true)
    (c/add-function (create-exp-cdf 1.0) 0 3
           :series-label "lambda=1.0")
    (c/add-function (create-exp-cdf 2.0) 0 3
           :series-label "lambda=2.0")
    (i/view)
  )            
)

(defn babyboom
  []
  (let [times (->> (iio/read-dataset "data/babyboom.dat" :skip 59 :delim \space)
                   (i/$ 3)
              )
        diffs (map - (rest times) times)
        f (create-cdf-f diffs)
        ccdf (fn [x] (- 1.0 (f x)))
       ]
       (-> (c/function-plot f 0 200
              :x-label "waiting time in min"
              :y-label "cdf"
              :title "cdf of waiting times"
              :series-label "cdf"
              :legend true
           )
           (c/add-function (create-exp-cdf 0.0306) 0 200
                 :series-label "exp cdf with lambda=0.0306")
           (i/view)
       )       
       (-> (c/function-plot ccdf 0 200
              :x-label "waiting time in min"
              :y-label "log ccdf"
              :title "ccdf of waiting times"
           )
           (c/set-axis :y (c/log-axis :label "Log probability"))
           (i/view)
       )
  )
)

