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

(defn show-norm-cdfs
  []
  (-> (c/function-plot #(s/cdf-normal % :mean 1.0 :sd 0.5) -1 4
           :x-label "x"
           :y-label "cdf"
           :series-label ":mean 1 :sd 0.5"
           :title "different normal cdfs"
           :legend true)
    (c/add-function #(s/cdf-normal % :mean 2.0 :sd 0.4) -1 4
           :series-label ":mean 2 :sd 0.4")
    (c/add-function #(s/cdf-normal % :mean 3.0 :sd 0.3) -1 4
           :series-label ":mean 3 :sd 0.3")
    (i/view)
  )            
)

(defn show-weight-with-norm-cdf
   [data]
   (let [first-children (->> data
                             (i/$where {:prglngth {:gt 27}})
                             (select-filtered :totalwgt_kg)
                        )
         f-first (create-cdf-f first-children)
        ]
        (-> (c/function-plot f-first 0 7
                   :x-label "kg"
                   :y-label "cdf"
                   :series-label "cdf weight children"
                   :legend true)
            (c/add-function #(s/cdf-normal % :mean 3.30 :sd 0.56) 0 7
                   :series-label "normal :mean 3.30 :sd 0.56")
            (i/view)
        )
   )
)


