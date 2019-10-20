(ns chapter1.data
  (:require [clojure.java.io :as io]
            [incanter.core :as i]
            [incanter.io :as iio]

  )
)

(defn open-dct-file
   []
   (->> (io/reader "data/2002FemPreg.dct")
        (line-seq)
        (rest)
        (butlast)
   )
)

(def type2parser
   {"byte" #(Integer/parseInt %)
    "int"  #(Integer/parseInt %)
    "float" #(Double/parseDouble %)
    "double" #(Double/parseDouble %)
   }
)

(defn parse-dct-line
  [line]
  (let [words   (clojure.string/split line #" +")
        wstart  (nth words 1)
        start   (Integer/parseInt (subs wstart 8 (dec (count wstart))))
        type    (nth words 2)
        wlength (nth words 4)
        length  (Integer/parseInt (subs wlength 1 (dec (count wlength))))]
     {:typeparser (get type2parser type str)
      :name (keyword (nth words 3))
      :start (dec start) ;start 1-base
      :end (+ (dec start) length) ;start 1-based
     } 
  )
)


(defn create-dat-line-parser-from-dct
  []
  (map parse-dct-line (open-dct-file))
)

(defn open-dat-file
   []
   (->> (io/reader "data/2002FemPreg.dat")
        (line-seq)
   )
)

(defn extract-entry-from-line
    [line extractor parsed]
    (let [s (->> (subs line (:start extractor) (:end extractor))
                 (clojure.string/trim)
            )
          val (if (empty? s) 
                  nil 
                  ((:typeparser extractor) s)
              )
         ]
         (conj parsed val)
    )
)


(defn parse-dat-line
  [line dat-line-parser]
  (reduce #(extract-entry-from-line line %2 %1) [] dat-line-parser)
)

(defn read-dat-file
  []
  (let [parser (create-dat-line-parser-from-dct)
        data (open-dat-file)]
       (i/dataset (map :name parser) (map #(parse-dat-line % parser) data))
  )    
)

(defn read-dat-file-filtered
  ([] (read-dat-file-filtered #{:caseid :prglngth :outcome 
                                :pregordr :birthord :birthwgt_lb
                                :birthwgt_oz :agepreg :finalwgt})
  )
  ([features]
    (let [parser (create-dat-line-parser-from-dct)
          filtered_parser (filter #(contains? features (:name %)) parser)
        data (open-dat-file)]
       (i/dataset (map #(:name %) filtered_parser) (map #(parse-dat-line % filtered_parser) data))
    )
  )    
)

(defn clean-dataset
   [dataset]
   (-> dataset
       (i/transform-col :agepreg #(if (some? %) (/ % 100.0) Double/NaN))
       (i/transform-col :birthwgt_lb #(if (contains? #{97 98 99 51 nil} %) Double/NaN %))
       (i/transform-col :birthwgt_oz #(if (contains? #{97 98 99 nil} %) Double/NaN %))
   )
)

(defn prepare-dataset
   [dataset]
   (->> (clean-dataset dataset)
        (i/add-derived-column :totalwgt_lb [:birthwgt_lb :birthwgt_oz] #(+ %1 (/ %2 16.0)))
        (i/add-derived-column :totalwgt_kg [:totalwgt_lb] #(* % 0,453592))
   )
)

(defn load-clean-dataset
   []
   (prepare-dataset (read-dat-file-filtered))
)


(defn select-filtered
   [col-name dataset]
   (->> dataset
        (i/$where {col-name {:$fn #(Double/isFinite %)}}) 
        (i/$ col-name)
   )
)
   

