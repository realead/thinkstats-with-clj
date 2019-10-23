# thinkstats-with-clj

my try to work through think stats using clojure

## Think Stats 2. edition

The pdf of the book can be downloaded here: https://greenteapress.com/wp/think-stats-2e/

### Dependencies
   
   * Cojure
   * leiningen (https://leiningen.org/)
   * some data sets need to be downloaded

### Usage

   * Download/process files as noted in thinkstats2e/data/README.md
   * `cd thinkstats2e`
   * `lein run` for running all examples from all chapters
   * `lein run chX chY ... chZ` for running all examples from chapters X,Y and Z
   * for running only some examples use `lein repl`, and now
       - `(chX/<Example_Y>)` to run an example y from chapter x, which doesn't need input
       - use `(def preg_data (load-clean-dataset :NAME))` to load filtered/cleaned one of the following datasets: FemPreg2002, FemResp2002
       - use `(read-dat-file :NAME)` to read/load raw data for the names above
       - `(chX/<Example_Y> preg_data)` to run an example y, which needs the input
       - `use 'thinkstats2e.chapter1 :reload)` to reload chapter1.clj

