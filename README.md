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
       - `(ex-1-xx)` to run an example
       - `(use 'chapter1.examples :reload)` to reload examples.clj
