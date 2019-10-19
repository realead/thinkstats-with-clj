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
      :name (nth words 3)
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

