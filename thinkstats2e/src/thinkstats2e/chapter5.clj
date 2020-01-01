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


(defn create-pareto-ccdf
  [xm alpha]
  (fn [x] (i/pow (/ x xm) (- alpha)))
)

(defn create-pareto-cdf
  [xm alpha]
  (fn [x] (- 1.0 (i/pow (/ x xm) (- alpha))))
)

(defn create-pareto-icdf
  [xm alpha]
  (fn [x] (* xm (i/exp (/ (i/log (- 1.0 x)) (- alpha)))))
)


(defn show-pareto-cdfs
  []
  (-> (c/function-plot (create-pareto-cdf 0.5 0.5) 0.5 3
           :x-label "x"
           :y-label "cdf"
           :series-label "xm=0.5 alpha=0.5"
           :legend true
           :title "pareto cdfs")
    (c/add-function (create-pareto-cdf 0.5 1) 0.5 3
           :series-label "xm=0.5 alpha=1.0")
    (c/add-function (create-pareto-cdf 0.5 2) 0.5 3
           :series-label "xm=0.5 alpha=2.0")
    (i/view)
  )            
)

(defn show-pareto-ccdfs
  []
  (-> (c/function-plot (create-pareto-ccdf 0.5 0.5) 0.5 3
           :x-label "x"
           :y-label "cdf"
           :series-label "xm=0.5 alpha=0.5"
           :legend true
           :title "pareto ccdfs")
    (c/add-function (create-pareto-ccdf 0.5 1) 0.5 3
           :series-label "xm=0.5 alpha=1.0")
    (c/add-function (create-pareto-ccdf 0.5 2) 0.5 3
           :series-label "xm=0.5 alpha=2.0")
    (c/set-axis :x (c/log-axis :label "Log x"))
    (c/set-axis :y (c/log-axis :label "Log ccdf"))
    (i/view)
  )            
)

(defn analyze-populations
   []
   (let [series (->> (iio/read-dataset "data/PEP_2012_PEPANNRES_with_ann.csv" :skip 2)
                     (i/$ 7)
                     (filter #(> % 0))
                )
         sorted-series (sort series)
         cdf-f (create-cdf-f series)
         ccdef-f #(max 1e-16 (- 1.0 (cdf-f %)))
         x (sort (s/sample-normal (count series) :mean 0 :sd 1.0))
        ]
        ;pareto
        (-> (c/function-plot ccdef-f  1 1e7
               :series-label "data"
               :legend true
               :title "comparison with pareto model")
            (c/add-function (create-pareto-ccdf 5000 1.4) 5000 1e7
              :series-label "pareto model xm=5000 alpha=1.4")
            (c/set-axis :x (c/log-axis :label "Log city population"))
            (c/set-axis :y (c/log-axis :label "Log ccdf"))
            (c/set-y-range 1e-10 1)
            (i/view)
        )
        ;normal 
        (let  [ series-mean (s/mean series)
                series-sd   (s/sd series)
              ]
            (-> (c/xy-plot x sorted-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with normal model"
                       :legend true)
                (c/add-function #(+ (* series-sd %) series-mean) -4 4
                     :series-label "normal model")
                (i/view)
            )
        )
        ;lognormal
        (let  [ log-series (i/log sorted-series)
                log-series-mean (s/mean log-series)
                log-series-sd   (s/sd log-series)
              ]
            (-> (c/xy-plot x log-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with lognormal model"
                       :legend true)
                (c/add-function #(+ (* log-series-sd %) log-series-mean) -4 4
                     :series-label "lognormal model")
                (i/view)
            )
        )
    )
)

;5.6

(defn create-exp-icdf
  [alpha]
  (fn [p] (/ (i/log (- 1.0 p)) (- alpha)))
)

(defn show-created-randoms
   []
   (let [alpha 2.0
         icdf (create-exp-icdf alpha)
         series (repeatedly 1000 #(random-from-icdf-f icdf))
         f-experimental (create-cdf-f series)
         f-theory (create-exp-cdf alpha)
        ]
          (-> (c/function-plot f-theory 0 5
                   :x-label "x"
                   :y-label "cdf"
                   :series-label "theory"
                   :legend true
                   :title "comparison theory vs. random distribution")
            (c/add-function f-experimental 0 5
                   :series-label "random distribution")
            (i/view)
          ) 
   )
)

;;exercises 

(defn ex-5-1
  []
  (let [
        f (fn [x] (s/cdf-normal x :mean 178 :sd 7.7))
       ]
       (println "probabilty for bluemen" (-  (f 185.42) (f 177.80)))
  )
)



(defn ex-5-2
  []
  (let [xm 1.0
        alpha 1.7
        cdf-f (create-pareto-cdf xm alpha)
        icdf-f (create-pareto-icdf xm alpha)
        mean-theory (* xm alpha (/ 1.0 (- alpha 1.0)))
       ]
      (-> (c/function-plot cdf-f 1 5
               :x-label "x"
               :y-label "cdf"
               :series-label "xm=1.0 alpha=1.7"
               :legend true
               :title "pareto world")
        (i/view)
      )  
      (println "median:" (* xm (i/exp (/ (i/log 2) alpha))))
      (println "mean theory:" mean-theory) 
      (println "mean experiment:" (s/mean (repeatedly 10000 #(random-from-icdf-f icdf-f))))
      (println "smaller than mean:" (cdf-f mean-theory))
      (let [p-one-smaller-km (cdf-f 1e3)
            p-all-smaller (i/pow p-one-smaller-km 7e9)
            p-at-least-one-bigger (- 1.0 p-all-smaller)
           ]
           (println "prob at least one of 7e9 bigger than 1km:" p-at-least-one-bigger) 
      )
      (let [p-one-smaller (cdf-f 1e6)
            p-all-smaller (i/pow p-one-smaller 7e9)
            p-at-least-one-bigger (- 1.0 p-all-smaller)
           ]
           (println "instead of mean (complicted to calculate), median as estimate: prob at least one of 7e9 bigger than 1e6m:" p-at-least-one-bigger) 
      )
  )         
)

(defn create-weibull-ccdf
  [alpha k]
  (fn [x] (i/exp (- (i/pow (/ x alpha) k)
                 )
          )
  )
)

(defn create-weibull-cdf
  [alpha k]
  (let [ccdf (create-weibull-ccdf alpha k)]
      (fn [x] (- 1.0 (ccdf x)))
  )
)

(defn ex-5-3
   []
   ;; just show cdfs:
   (-> (c/function-plot (create-weibull-cdf 10 2)  0 20
               :x-label "x"
               :y-label "cdf"
               :series-label "weibull alpha=10 k=2"
               :legend true
               :title "weibull's cdfs")
        (c/add-function (create-weibull-cdf 10 3) 0 20
          :series-label "weibull alpha=10 k=3")
        (c/add-function (create-weibull-cdf 5 2) 0 20
          :series-label "weibull alpha=5 k=2")
        (i/view)
    )
   ;; show ccdfs:
   (let [f1 #(- (i/log ((create-weibull-ccdf 10 2) %)))
         f2 #(- (i/log ((create-weibull-ccdf 10 3) %)))
         f3 #(- (i/log ((create-weibull-ccdf 5 2) %)))
        ]
       (-> (c/function-plot f1  0 20
                   :series-label "weibull alpha=10 k=2"
                   :legend true
                   :title "weibull's log ccdfs")
            (c/add-function f2 0 20
              :series-label "weibull alpha=10 k=3")
            (c/add-function f3 0 20
              :series-label "weibull alpha=5 k=2")
            (c/set-axis :x (c/log-axis :label "Log x"))
            (c/set-axis :y (c/log-axis :label "Log log ccdf"))
            (i/view)
        )
   )
)

(defn ex-5-4
  []
  (let [times (->> (iio/read-dataset "data/babyboom.dat" :skip 59 :delim \space)
                   (i/$ 3)
              )
        diffs (map - (rest times) times)
        f (create-cdf-f diffs)
        sim-diffs (s/sample-exp (count diffs) :rate (/ 1.0 33.0))
        f2 (create-cdf-f sim-diffs)
        sim-diffs2 (s/sample-exp (count diffs) :rate (/ 1.0 33.0))
        f3 (create-cdf-f sim-diffs2)
       ]
       (-> (c/function-plot f 0 200
              :x-label "waiting time in min"
              :y-label "cdf"
              :title "cdf of waiting times"
              :series-label "cdf from data"
              :legend true
           )
           (c/add-function f2 0 200
                 :series-label "cdf simulated")
           (c/add-function f3 0 200
                 :series-label "cdf simulated 2")
           (i/view)
       )   
  )
)



(defn ex-5-5-0
  []
  (let [series (->> (iio/read-dataset "data/mystery0.dat")
                    (i/$ 0)
               )
       ]
       (-> (c/histogram  series
                        :nbins 20         
                        :density true 
                        :title "mystery0"
           )
           (i/view)
       ) 
      (-> (c/function-plot (create-cdf-f series) -1 101
                  :x-label "values"
                  :y-label "cdf"
                  :title "mystery0: uniform distribution 0..100"
           )
           (i/view)
     ) 
     (println "mystery0: uniform distribution")
  )
)

(defn ex-5-5-1
  []
  (let [sorted-series (->> (iio/read-dataset "data/mystery1.dat")
                    (i/$ 0)
                    (sort)
               )
        x (sort (s/sample-normal (count sorted-series) :mean 0 :sd 1.0))
       ]
       (-> (c/histogram  sorted-series
                        :nbins 20         
                        :density true 
                        :title "mystery1"
           )
           (i/view)
       ) 
        ;normal 
        (let  [ series-mean (s/mean sorted-series)
                series-sd   (s/sd sorted-series)
              ]
            (-> (c/xy-plot x sorted-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with normal model"
                       :legend true)
                (c/add-function #(+ (* series-sd %) series-mean) -4 4
                     :series-label "normal model")
                (i/view)
            )
        )
        ;lognormal
        (let  [ log-series (i/log sorted-series)
                log-series-mean (s/mean log-series)
                log-series-sd   (s/sd log-series)
              ]
            (-> (c/xy-plot x log-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with lognormal model"
                       :legend true)
                (c/add-function #(+ (* log-series-sd %) log-series-mean) -4 4
                     :series-label "lognormal model")
                (i/view)
            )
        ) 
       (println "mystery1: neither normal nor lognormal (cheated: from histrogram -> triangular)")
  )
)

(defn ex-5-5-2
  []
  (let [sorted-series (->> (iio/read-dataset "data/mystery2.dat")
                    (i/$ 0)
                    (sort)
               )
        rate (/ 1.0 (s/mean sorted-series))
       ]
       (-> (c/histogram  sorted-series
                        :nbins 20         
                        :density true 
                        :title "mystery2"
           )
           (i/view)
       ) 
       (-> (c/function-plot (create-cdf-f sorted-series) 0 350
                  :x-label "values"
                  :y-label "cdf"
                  :title "mystery2: exp-distribution"
                  :series-label "cdf-data"
                  :legend true
           )
           (c/add-function #(s/cdf-exp % :rate rate) 0 350
                     :series-label (str "cdf-exp with rate= " rate))
           (i/view)
       ) 
       (println "mystery2: exponential distribution")
  )
)


(defn ex-5-5-3
  []
  (let [sorted-series (->> (iio/read-dataset "data/mystery3.dat")
                    (i/$ 0)
                    (sort)
               )
        x (sort (s/sample-normal (count sorted-series) :mean 0 :sd 1.0))
        series-mean (s/mean sorted-series)
        series-sd   (s/sd sorted-series)
       ]
       (-> (c/histogram  sorted-series
                        :nbins 20         
                        :density true 
                        :title "mystery3"
           )
           (i/view)
       ) 
       (-> (c/function-plot (create-cdf-f sorted-series) -25 125
                  :x-label "values"
                  :y-label "cdf"
                  :series-label "cdf-data"
                  :legend true
           )
           (c/add-function #(s/cdf-normal % :mean series-mean :sd series-sd) -25 125
                     :series-label (str "cdf-normal with mean= " series-mean " sd=" series-sd))
           (i/view)
       ) 
        ;normal 
        (let  [ series-mean (s/mean sorted-series)
                series-sd   (s/sd sorted-series)
              ]
            (-> (c/xy-plot x sorted-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with normal model"
                       :legend true)
                (c/add-function #(+ (* series-sd %) series-mean) -4 4
                     :series-label (str "normal model with mean= " series-mean " sd=" series-sd))
                (i/view)
            )
        )
       (println "normal model with mean= " series-mean " sd=" series-sd)
  )
)

(defn ex-5-5-4
  []
  (let [sorted-series (->> (iio/read-dataset "data/mystery4.dat")
                    (i/$ 0)
                    (sort)
               )
        x (sort (s/sample-normal (count sorted-series) :mean 0 :sd 1.0))
        series-mean (s/mean sorted-series)
        series-sd   (s/sd sorted-series)
       ]
       (-> (c/histogram  sorted-series
                        :nbins 20         
                        :density true 
                        :title "mystery4"
           )
           (i/view)
       ) 
       (-> (c/function-plot (create-cdf-f sorted-series) -25 125
                  :x-label "values"
                  :y-label "cdf"
                  :series-label "cdf-data"
                  :legend true
           )
           (c/add-function #(s/cdf-normal % :mean series-mean :sd series-sd) -100 200
                     :series-label (str "cdf-normal with mean= " series-mean " sd=" series-sd))
           (i/view)
       ) 
        ;normal 
        (let  [ series-mean (s/mean sorted-series)
                series-sd   (s/sd sorted-series)
              ]
            (-> (c/xy-plot x sorted-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with normal model"
                       :legend true)
                (c/add-function #(+ (* series-sd %) series-mean) -4 4
                     :series-label (str "normal model with mean= " series-mean " sd=" series-sd))
                (i/view)
            )
        )
        ;lognormal
        (let  [ log-series (i/log sorted-series)
                log-series-mean (s/mean log-series)
                log-series-sd   (s/sd log-series)
              ]
            (-> (c/xy-plot x log-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with lognormal model"
                       :legend true)
                (c/add-function #(+ (* log-series-sd %) log-series-mean) -4 4
                     :series-label "lognormal model")
                (i/view)
            )
        ) 
       (println "lognormal model with mean= " series-mean " sd=" series-sd)
  )
)

(defn ex-5-5-5
  []
  (let [sorted-series (->> (iio/read-dataset "data/mystery5.dat")
                    (i/$ 0)
                    (sort)
               )
        x (sort (s/sample-normal (count sorted-series) :mean 0 :sd 1.0))
        series-mean (s/mean sorted-series)
        series-sd   (s/sd sorted-series)
        cdf-f (create-cdf-f sorted-series)
        
       ]
       (-> (c/histogram  sorted-series
                        :nbins 20         
                        :density true 
                        :title "mystery5"
           )
           (i/view)
       ) 
       (-> (c/function-plot cdf-f -0 125
                  :x-label "values"
                  :y-label "cdf"
                  :series-label "cdf-data"
                  :legend true
           )
           (i/view)
       ) 
       (-> (c/function-plot #(- 1.0 (cdf-f %)) -0 125
                  :x-label "values"
                  :y-label "cdf"
                  :series-label "ccdf-data"
                  :legend true
           )
           (c/set-axis :x (c/log-axis :label "Log x"))
           (c/set-axis :y (c/log-axis :label "Log ccdf"))
           (i/view)
       ) 
       ;normal 
       (let  [ series-mean (s/mean sorted-series)
                series-sd   (s/sd sorted-series)
              ]
            (-> (c/xy-plot x sorted-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with normal model"
                       :legend true)
                (c/add-function #(+ (* series-sd %) series-mean) -4 4
                     :series-label (str "normal model with mean= " series-mean " sd=" series-sd))
                (i/view)
            )
        )
        ;lognormal
        (let  [ log-series (i/log sorted-series)
                log-series-mean (s/mean log-series)
                log-series-sd   (s/sd log-series)
              ]
            (-> (c/xy-plot x log-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with lognormal model"
                       :legend true)
                (c/add-function #(+ (* log-series-sd %) log-series-mean) -4 4
                     :series-label "lognormal model")
                (i/view)
            )
        ) 
       (println "pareto distribution (from loglog of ccdf)")
  )
)

(defn ex-5-5-6
  []
  (let [sorted-series (->> (iio/read-dataset "data/mystery6.dat")
                    (i/$ 0)
                    (sort)
               )
        x (sort (s/sample-normal (count sorted-series) :mean 0 :sd 1.0))
        series-mean (s/mean sorted-series)
        series-sd   (s/sd sorted-series)
        cdf-f (create-cdf-f sorted-series)
        
       ]
       (-> (c/histogram  sorted-series
                        :nbins 20         
                        :density true 
                        :title "mystery6"
           )
           (i/view)
       ) 
       (-> (c/function-plot (create-cdf-f sorted-series) -25 125
                  :x-label "values"
                  :y-label "cdf"
                  :series-label "cdf-data"
                  :legend true
           )
           (c/add-function #(s/cdf-normal % :mean series-mean :sd series-sd) -25 125
                     :series-label (str "cdf-normal with mean= " series-mean " sd=" series-sd))
           (i/view)
       )
       (let [f #(- (i/log (- 1.0  (cdf-f %))
                   )
                )
             f2 #(- (i/log (- 1.0 (s/cdf-normal % :mean series-mean :sd series-sd))
                    )
                 )
            ]
            (-> (c/function-plot f  40 70
                           :series-label "data"
                           :legend true
                           :title "log ccdfs")
                (c/add-function f2 40 70
                       :series-label "normal")
                (c/set-axis :x (c/log-axis :label "Log x"))
                (c/set-axis :y (c/log-axis :label "Log log ccdf"))
                (i/view)
             )
       )
       ;normal 
       (let  [ series-mean (s/mean sorted-series)
                series-sd   (s/sd sorted-series)
              ]
            (-> (c/xy-plot x sorted-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with normal model"
                       :legend true)
                (c/add-function #(+ (* series-sd %) series-mean) -4 4
                     :series-label (str "normal model with mean= " series-mean " sd=" series-sd))
                (i/view)
            )
        )
        ;lognormal
        (let  [ log-series (i/log sorted-series)
                log-series-mean (s/mean log-series)
                log-series-sd   (s/sd log-series)
              ]
            (-> (c/xy-plot x log-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with lognormal model"
                       :legend true)
                (c/add-function #(+ (* log-series-sd %) log-series-mean) -4 4
                     :series-label "lognormal model")
                (i/view)
            )
        ) 
       (println "not quite the normal distribution (cheated: weibull)")
  )
)

(defn ex-5-5-7
  []
  (let [sorted-series (->> (iio/read-dataset "data/mystery7.dat")
                    (i/$ 0)
                    (sort)
               )
        x (sort (s/sample-normal (count sorted-series) :mean 0 :sd 1.0))
        series-mean (s/mean sorted-series)
        series-sd   (s/sd sorted-series)
        cdf-f (create-cdf-f sorted-series)
        
       ]
       (-> (c/histogram  sorted-series
                        :nbins 20         
                        :density true 
                        :title "mystery6"
           )
           (i/view)
       ) 
       (-> (c/function-plot (create-cdf-f sorted-series) -25 125
                  :x-label "values"
                  :y-label "cdf"
                  :series-label "cdf-data"
                  :legend true
           )
           (c/add-function #(s/cdf-normal % :mean series-mean :sd series-sd) -25 125
                     :series-label (str "cdf-normal with mean= " series-mean " sd=" series-sd))
           (i/view)
       )
       ;normal 
       (let  [ series-mean (s/mean sorted-series)
                series-sd   (s/sd sorted-series)
              ]
            (-> (c/xy-plot x sorted-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with normal model"
                       :legend true)
                (c/add-function #(+ (* series-sd %) series-mean) -4 4
                     :series-label (str "normal model with mean= " series-mean " sd=" series-sd))
                (i/view)
            )
        )
        ;lognormal
        (let  [ log-series (i/log sorted-series)
                log-series-mean (s/mean log-series)
                log-series-sd   (s/sd log-series)
              ]
            (-> (c/xy-plot x log-series
                       :x-label "standard normal sample"
                       :y-label "sample sizes"
                       :series-label "data"
                       :title "comparison with lognormal model"
                       :legend true)
                (c/add-function #(+ (* log-series-sd %) log-series-mean) -4 4
                     :series-label "lognormal model")
                (i/view)
            )
        ) 
       (println "(cheated: gumbel)")
  )
)


(defn ex-5-6
  []
  (let [hist-y [4204 4729 6982 7157 7131 6740 6354 5832 5547 5254 5102 4256 4356 3949 3756 3414 3326 2643 2678 2223 2606 1838 1986 1464 1596 1327 1253 1140 1119 920 1143 805 731 575 616 570 502 364 432 378]
        hist-x (range 2500 199999 5000)

        n-200-250 2549
        n-251     2911
        cdf (-> (into (sorted-map)(map vector hist-x hist-y))
                (normalize-frequencies)
                (cdf-from-pmf) 
            )
        cdf-sum (float (apply + hist-y))
        all-sum (float (apply +  (conj hist-y n-200-250 n-251)))
        factor (/ cdf-sum all-sum)
        ccdf-f #(- 1.0 (* factor (eval-cdf % cdf)))
       ]
       (-> (c/xy-plot hist-x hist-y
                       :x-label "income"
                       :y-label "number of people"
                       :series-label "data"
                       :title "histogram"
                       :legend true)
           (i/view)
       )
       (-> (c/function-plot ccdf-f 1 200000
                  :x-label "values"
                  :y-label "cdf"
                  :series-label "ccdf-data"
                  :legend true
           )
           (c/add-function (create-pareto-ccdf 2e4 0.7) 2e4 3e5
                           :series-label "pareto model xm=2e5 lambda=0.7")
           (c/add-function (create-pareto-ccdf 2e4 0.9) 2e4 3e5
                           :series-label "pareto model xm=2e5 lambda=0.9")
           (c/set-axis :x (c/log-axis :label "Log x"))
           (c/set-axis :y (c/log-axis :label "Log ccdf"))
           (i/view)
       )
       (println "pareto model is good approximation for incomes > 2e4") 
  )
)
