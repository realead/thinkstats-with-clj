# thinkstats-with-clj

my try to work through think stats using clojure

## Think Stats

The pdf of the book can be downloaded here: https://greenteapress.com/wp/think-stats-2e/

### Dependencies
   
   * Cojure
   * leiningen (https://leiningen.org/)
   * some data sets need to be downloaded

### Chapter 1

   * Download/process files as noted in chapter1/data/README.md
   * `cd chapter1`
   * `lein run` for running all examples
   * for running only some examples use `lein repl`, and now
       - `(<Example_X>)` to run an example x, which doesn't need input
       - use `(def preg_data (read-dat-file))` to load the data
       - use `(def preg_data (load-clean-dataset))` to faster load only chosen fields (see code for which they are), which are also cleaned up in addition
       - `(<Example_Y> preg_data)` to run an example y, which needs the input
       - `(use 'chapter1.examples :reload)` to reload examples.clj
