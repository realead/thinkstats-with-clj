(ns thinkstats2e.utils

)

(defn my_pow [x n]
  (reduce * (repeat n x)))

;;;; pmf-functionality

(defn normalize-frequencies
  [freqs]
  (let [norm (apply + (vals freqs))]
      (->> (for [[k v] freqs] 
                 [k (double (/  v norm))])
           (into (sorted-map))
      ) 
  )
)

(defn mean-of-pmf
     [pmf]
     (reduce +
         (for [[k v] pmf]
              (* k v)
         )
     )
)

(defn variance-of-pmf
     [pmf]
     ;no so good: traverses series twice
     (let [m (mean-of-pmf pmf)]
           (reduce +   
               (for [[k p] pmf
                    :let [d (- k m)]
                   ]
                   (* p d d)
               )
           )
     )
)

(defn pmf
   [series]
   (-> series
       (frequencies)
       (normalize-frequencies)
   )
)

(defn cdf-from-pmf
   [p]
   (let [m (apply min (keys p)) ]
        (->> (reductions (fn [f s] [(first s) (+ (second f) (second s))]) [(dec m) 0] p)
             (into (sorted-map))
        )

   )
)


(defn cdf
   [series]
   (let [p (pmf series)
         m (apply min (keys p)) ]
        (->> (reductions (fn [f s] [(first s) (+ (second f) (second s))]) [(dec m) 0] p)
             (into (sorted-map))
        )

   )
)

(defn eval-cdf
   [x cdf]
   (let [s (rsubseq cdf <= x)]
        (if (empty? s)
            0.0
            (second (first s))
         )
   )
)

(defn create-cdf-f
  [series]
  (fn [x] (min 1.0 (eval-cdf x (cdf series))))
)

(defn create-ccdf-f
  [series]
  (fn [x] (max 0.0 (- 1.0 (eval-cdf x (cdf series)))))
)

; not fastest but works for now:
(defn value-cdf
   [x cdf]
   (-> (drop-while #(< (second %) x) cdf)
       (first)
       (first)
   )
)

(defn percentil-rank-cdf
   [x cdf]
   (* 100 (eval-cdf x cdf))
)

(defn percentil-cdf
   [p cdf]
   (value-cdf (/ p 100) cdf)
)

(defn median-cdf
   [cdf]
   (value-cdf 0.5 cdf)
)

(defn iqr-cdf
  [cdf]
  (- (percentil-cdf 75 cdf) (percentil-cdf 23 cdf))
)

(defn random-cdf
   [cdf]
   (percentil-cdf (rand-int 101) cdf)
)

(defn random-from-icdf-f
   [icdf]
   (icdf (rand))
)



 
