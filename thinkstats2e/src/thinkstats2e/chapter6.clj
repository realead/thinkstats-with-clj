(ns thinkstats2e.chapter6
  (:require [thinkstats2e.data :refer :all]
            [thinkstats2e.utils :refer :all]
            [incanter.core :as i]
            [incanter.io :as iio]
            [incanter.charts :as c]
            [incanter.stats :as s]
  )
)


;; 6.1



(defn pmf-from-normal
  [mu sd]
  (let [d (* 6 sd 0.01)
        xs (for [x (range -50 51)] (+ mu (* d x)))      
        ys (map #(s/pdf-normal % :mean mu :sd sd) xs)
       ]
       (normalize-frequencies (zipmap xs ys))
  )
)

(defn calc-norm-density
  []
  (let [mu  163.0
        var  52.8
        s   (i/sqrt var)
        my-pmf (pmf-from-normal mu s)]
    (println (s/pdf-normal (+ mu s) :mean mu :sd s))
    (-> (c/function-plot #(s/pdf-normal % :mean mu :sd s) 
                          (- mu (* 4 s)) 
                          (+ mu (* 4 s))
           :x-label "x"
           :y-label "pdf"
           :series-label "normal"
           :legend true)
        (i/view)
    )
    (-> (c/bar-chart (keys my-pmf) (vals my-pmf) 
                        :title "pmf"
                        :x-label "x"
                        :y-label "pmf"
                        :legend true
                        :series-label "pmf of normal")
        (i/view)
    )
  )
)

;; 6.2

;; 6.7

(defn raw-moment 
  [vs k]
  (let [s (apply + (map #(my_pow % k) vs))]
      (/ s (double (count vs)))
  )
)


(defn central-moment 
  [vs k]
  (let [xbar (raw-moment vs 1)]
       
       (raw-moment (map #(- % xbar) vs) k)
  )
)

(defn example-central-moment
  []
  (central-moment [1 2 3] 2) 
)

(defn standard-moment 
  [vs k]
  (let [sigma2 (central-moment vs 2)
        moment (central-moment vs k)]
       
       (/ moment (my_pow (Math/sqrt sigma2) k))
  )
)

(defn skewness 
  [vs]
  (standard-moment vs 3)
)


(defn example-skewness
  []
  (skewness [1 2 3]) 
)


;; 6-1

(defn w-mean
  [vals freqs]
  (let [n (reduce + freqs)
        s (reduce + (map * vals freqs))
       ]
       (/ s (double n))  
  )
)


(defn w-median
  ([vals freqs]
    (w-median vals freqs (/ (reduce + freqs) 2))
  )
  ([vals freqs n]
      (if (> n (first freqs))
          (recur (rest vals) (rest freqs) (- n (first freqs))) 
          (first vals)    
      )
  )
)

(defn w-raw-moment
  [vals freqs k]
  (let [n (reduce + freqs)
        s (reduce + (map * (map #(my_pow % k) vals) freqs))
       ]
       (/ s (double n))  
  )
)

(defn w-central-moment 
  [vals freqs k]
  (let [xbar (w-mean vals freqs)]       
       (w-raw-moment (map #(- % xbar) vals) freqs k)
  )
)

(defn w-standard-moment 
  [vals freqs k]
  (let [sigma2 (w-central-moment vals freqs 2)
        moment (w-central-moment vals freqs k)]
       
       (/ moment (my_pow (Math/sqrt sigma2) k))
  )
)

(defn w-skewness 
  [vals freqs]
  (w-standard-moment vals freqs 3)
)

(defn w-pearson-skewness 
  [vals freqs]
  (let [sigma (Math/sqrt (w-central-moment vals freqs 2))
        mean  (w-mean vals freqs)
        median (w-median vals freqs)
        nom (* 3 (- mean median))]      
       (/ nom sigma)
  )
)


(defn ex-6-1
  [data]
  (let [freqs (i/$ :col1 data)
        vals  (i/$ :col2 data)
       ]
       {:mean (w-mean vals freqs) :median (w-median vals freqs) :skewness (w-skewness vals freqs) :pearson-skewness (w-pearson-skewness vals freqs)}
  )
)

