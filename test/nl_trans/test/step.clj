(ns nl-trans.test.step
  (:use clojure.tools.trace)
  (:use [nl-trans.step])
  (:use [clojure.test]))

;; (trace-ns 'nl-trans.step)

(defn inside-interval [n]
  (and
   (<= -1 n)
   (>= 1.00001 n)
   ))

(deftest mk-rand-test
  (let [res (nl-trans.step/mk-rand)]
    (is (inside-interval res)
        )))

(deftest mk-point-test
  (let [[x y] (nl-trans.step/mk-point)]
    (is (inside-interval x))
    (is (inside-interval y))
    ))

(deftest mk-any-line-test
  (let [[[x1 y1] [x2 y2]] (nl-trans.step/mk-any-line)]
    (is (inside-interval x1))
    (is (inside-interval y1))
    (is (inside-interval x2))
    (is (inside-interval y2))
    ))

(deftest calc-line-w-test
  (is (= [14/3 5/2 -77/6] (nl-trans.step/calc-line-w [[3/2 7/3] [-1 7]])))
  )

(deftest normalize-by-x-test
  (is (= [[-1 7] [1 49/15]] (nl-trans.step/normalize-by-x 14/3 5/2 -77/6)))
  (is (= [[-1 23/7] [1 31/14]] (nl-trans.step/normalize-by-x 5/2 14/3 -77/6)))
  )

(deftest normalize-by-y-test
  (is (= [[23/7 -1] [31/14 1]] (nl-trans.step/normalize-by-y 14/3 5/2 -77/6)))
  (is (= [[7 -1] [49/15 1]] (nl-trans.step/normalize-by-y 5/2 14/3 -77/6)))
  )

(deftest calc-one-y-test
  (is (= 1 (nl-trans.step/calc-one-y [[-1 0] [1 0]] [0 0])))
  (is (= 1 (nl-trans.step/calc-one-y [[-1 0] [1 0]] [1 -0.0001])))
  (is (= -1 (nl-trans.step/calc-one-y [[-1 0] [1 0]] [1 0.0001])))

  (is (= 1 (nl-trans.step/calc-one-y [[0 -1] [0 1]] [0 0])))
  (is (= 1 (nl-trans.step/calc-one-y [[0 -1] [0 1]] [0.0001 0])))
  (is (= -1 (nl-trans.step/calc-one-y [[0 -1] [0 1]] [-0.0001 0])))
  (is (= 1 (nl-trans.step/calc-one-y [[-1 -1] [1 1]] [3 3])))

  (is (= 1 (nl-trans.step/calc-one-y [[-1 -1] [1 1]] [3 0])))
  (is (= 1 (nl-trans.step/calc-one-y [[-1 -1] [1 1]] [0 -1])))
  (is (= -1 (nl-trans.step/calc-one-y [[-1 -1] [1 1]] [0 1])))
  )

(deftest calc-y-test
  (is (= [-1] (nl-trans.step/calc-y [[-1 0] [1 0]] [[3 3]])))
  (is (= [1] (nl-trans.step/calc-y [[-1 -1] [1 1]] [[1 1]])))
  (is (= [1] (nl-trans.step/calc-y [[-1 -1] [1 1]] [[3.01 3]])))
  (is (= [1] (nl-trans.step/calc-y [[-1 0] [1 0]] [[-3 -3]])))
  )

(deftest split-points-test
  (is (= [[] [[3.01 3]]] (nl-trans.step/split-points [1] [[3.01 3]])))
  (is (= [[[0.1 -0.9]] [[3.01 3]]]
         (nl-trans.step/split-points [-1 1] [[0.1 -0.9] [3.01 3]])))
  )

(deftest calc-one-y2-test
  (is (= 1 (nl-trans.step/calc-one-y2 [1 0.21 0.33] [-1 0])))
  (is (= 1 (nl-trans.step/calc-one-y2 [1 0.25 0.25] [-1 -3])))
  (is (= -1 (nl-trans.step/calc-one-y2 [1 0.21 0.33] [-1 -3])))
  )

(deftest line-outside-test
  (is (= false (nl-trans.step/line-outside [[-1 -1] [1 1]])))
  (is (= true (nl-trans.step/line-outside [[-1 -1] [-1 1]])))
  (is (= false (nl-trans.step/line-outside [[0.21 0.33] [-1 -3]])))
  (is (= false (nl-trans.step/line-outside [[0.21 0.33] [-0.1 -0.3]])))
  (is (= true (nl-trans.step/line-outside [[-1.1 -1] [-1 1.1]])))
  )

(deftest reg-test
  (let [
        line [[-1 0.34843307003649615] [1 1.9429805981704393]]
        points [[-48 -76]
                [-37 -89]
                [65 -1]
                [6 -79]]
        ys (nl-trans.step/calc-y line points)
        _ (is (= ys [1 1 1 1]))
        act (nl-trans.step/reg ys points)
        ]
    (is (= act [0.01100025047260729 -0.015819429436061582]))
    ))

