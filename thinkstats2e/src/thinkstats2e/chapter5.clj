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

;5.2

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

;5.3

(defn show-normal-prob-plot
   []
   (let [x (sort (s/sample-normal 1000 :mean 0 :sd 1.0))
         y1 (sort (s/sample-normal 1000 :mean 0 :sd 1.0))
         y2 (sort (s/sample-normal 1000 :mean 1.0 :sd 1.0))
         y3 (sort (s/sample-normal 1000 :mean 5.0 :sd 2.0))
        ]
        (-> (c/xy-plot x y1
                   :x-label "standard normal sample"
                   :y-label "sample values"
                   :series-label ":mean 0 :sd 1.0"
                   :legend true)
            (c/add-lines x y2
                   :series-label ":mean 1.0 :sd 1.0")
            (c/add-lines x y3
                   :series-label ":mean 5.0 :sd 2.0")
            (i/view)
        )
   )
)

(defn show-normal-prob-plot-weights
   [data]
   (let [
         x-model (sort (s/sample-normal 1000 :mean 0 :sd 1.0))
         y-model (sort (s/sample-normal 1000 :mean 3.30 :sd 0.56))
         y-alive (->> data
                     (i/$where {:outcome 1})
                     (select-filtered :totalwgt_kg)
                     (sort)
                )
         x-alive (sort (s/sample-normal (count y-alive) :mean 0 :sd 1.0))

         y-full (->> data
                     (i/$where {:outcome 1 :prglngth {:gt 36}})
                     (select-filtered :totalwgt_kg)
                     (sort)
                )
         x-full (sort (s/sample-normal (count y-full) :mean 0 :sd 1.0))
        ]
        (-> (c/xy-plot x-model y-model
                   :x-label "standard normal sample"
                   :y-label "sample weigths kg"
                   :series-label "model-simulated"
                   :legend true)
            (c/add-lines x-alive y-alive
                   :series-label "alive")
            (c/add-lines x-full y-full
                   :series-label "alive and full")
            (c/add-function #(+ (* 0.56 %) 3.30) -4 4
                 :series-label "model analytical")
            (i/view)
        )
   )
)

;5.4

(defn show-lognormal
   []
   (let [
         data (->> (iio/read-dataset "data/weights.txt" :skip 1 :delim \space)
                   (i/$ 0)
                   (sort)
              )
         data-mean (s/mean data)
         data-sd   (s/sd data)
         log-data (map i/log data)
         log-data-mean (s/mean log-data)
         log-data-sd   (s/sd log-data)
         x (sort (s/sample-normal (count data) :mean 0 :sd 1.0))
        ]
        (-> (c/xy-plot x data
                   :x-label "standard normal sample"
                   :y-label "sample weigths"
                   :series-label "weights"
                   :legend true)
            (c/add-function #(+ (* data-sd %) data-mean) -4 4
                 :series-label "model weights")
            (i/view)
        )
        (-> (c/xy-plot x log-data
                   :x-label "standard normal sample"
                   :y-label "sample log weigths"
                   :series-label "log weights"
                   :legend true)
            (c/add-function #(+ (* log-data-sd %) log-data-mean) -4 4
                 :series-label "model log weights")
            (i/view)
        )
   )
)


