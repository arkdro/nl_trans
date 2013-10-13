(ns nl-trans.core
  {:doc "linear regression, non-linear transformation"}
  (:use [clojure.tools.cli :only [cli]])
  (:require clojure.string)
  (:use clojure.tools.trace)
  (:require nl-trans.misc)
  (:require nl-trans.step)
  (:gen-class)
  )

;; (trace-ns 'nl-trans.step)
;; (trace-vars nl-trans.step/calc-one-step)
;; (trace-vars nl-trans.step/nl-trans)
;; (trace-vars nl-trans.step/nl-trans-aux)
;; (trace-vars nl-trans.step/get-misclassified)
;; (trace-vars nl-trans.step/update-w)
;; (trace-vars nl-trans.step/is-misclassified)
;; (untrace-vars nl-trans.step/calc-diff-prob)
;; (untrace-vars nl-trans.step/calc-diff-prob-aux)

(defn call-calc [n cnt verbose pic]
  (if verbose
    (binding [*out* *err* nl-trans.misc/*verbose* 'true]
      (time (nl-trans.step/calc n cnt pic)))
    (nl-trans.step/calc n cnt pic)))

(defn print-result [res]
  (println "res: " res))

(defn -main [& args]
  (let [opts (cli
              args
              ["-v" "--[no-]verbose" :default false]
              ["-p" "--[no-]picture" :default false]
              ["-c" "--cnt" "Count of experiments"
               :parse-fn #(Integer. %)
               :default 1]
              ["-n" "--n" "N of points" :parse-fn #(Integer. %)
               :default 10])
        [options _ _] opts
        res (call-calc (:n options)
                       (:cnt options)
                       (:verbose options)
                       (:picture options))
        ]
    (print-result res)))

