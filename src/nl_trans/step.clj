(ns nl-trans.step
  (:use [incanter core stats charts])
  (:require [nl-trans.misc])
  )

(defn mk-filename [base i]
  (let [
        i-str (format "%04d" i)]
    (apply str ["reg-" base "-" i-str ".png"])))

(defn plot [[[x1 y1] [x2 y2]] base i]
  ;; ex1:
  ;; (doto (xy-plot [1 2 3] [4 5 6]) (add-points [1 2 3] [4.1 5.1 6.1]) (set-stroke-color java.awt.Color/black :series 0) (set-stroke-color java.awt.Color/red :series 1) view)

  ;; ex2:
  ;; (def c1 (xy-plot [-1 0.65] [-0.3 0.8])) ;; this is the first line
  ;; (set-x-range c1 -1 1)
  ;; (set-y-range c1 -1 1)
  ;; (set-stroke c1 :width 4 :dash 5)
  ;; (set-stroke-color c1 java.awt.Color/red :series 0) ;; for first line
  ;; (set-stroke-color c1 java.awt.Color/red :series 1) ;; for second dot
  ;; (set-stroke-color c1 java.awt.Color/red :series 2) ;; for third dot pack
  ;; (add-points c1 [0.88] [0.01]) ;; this is the second dot
  ;; (add-points c1 [0.15 0.55 0.95] [3.15 2.91 2.51]) ;; this is the 3rd pack
  ;; (save c1 "/tmp/c1-1.png" :width 600 :height 500)
  ;; (view c1)
  (let [fname (mk-filename base i)]
    (doto (xy-plot [x1 x2] [y1 y2])
      (set-x-range -1 1)
      (set-y-range -1 1)
      ;; (add-points [1 2 3] [4.1 5.1 6.1])
      (set-stroke-color java.awt.Color/black :series 0)
      ;; (set-stroke-color java.awt.Color/red :series 1)
      (save fname :width 300 :height 250)
      )))

(defn plot-one-square [[[x1 y1] [x2 y2] :as line]
                       neg-points
                       pos-points
                       base]
  (let [fname (mk-filename base 0)
        neg-xs (map first neg-points)
        neg-ys (map second neg-points)
        pos-xs (map first pos-points)
        pos-ys (map second pos-points)
        ]
    (doto (xy-plot [x1 x2] [y1 y2])
      (set-x-range -1 1)
      (set-y-range -1 1)
      (add-points neg-xs neg-ys)
      (add-points pos-xs pos-ys)
      (set-stroke-color java.awt.Color/black :series 0)
      (set-stroke-color java.awt.Color/red :series 1)
      (set-stroke-color java.awt.Color/green :series 2)
      (save fname :width 300 :height 250)
      ))
  :ok)

(defn plot-one-res-square-aux [[[x1 y1] [x2 y2] :as line]
                               neg-points pos-points base
                               [[rx1 ry1] [rx2 ry2] :as res-line]]
  (nl-trans.misc/log-val "l1:" ["x1" x1 "y1" y1 ", x2" x2 "y2" y2])
  (nl-trans.misc/log-val "l2:" ["rx1" rx1 "ry1" ry1 ", rx2" rx2 "ry2" ry2])
  (let [fname (mk-filename base 1)
        neg-xs (map first neg-points)
        neg-ys (map second neg-points)
        pos-xs (map first pos-points)
        pos-ys (map second pos-points)
        ]
    (doto (xy-plot [x1 x2] [y1 y2])
      (set-x-range -1 1)
      (set-y-range -1 1)
      (add-points neg-xs neg-ys)
      (add-points pos-xs pos-ys)
      (add-lines [rx1 rx2] [ry1 ry2])
      (set-stroke-color java.awt.Color/black :series 0)
      (set-stroke-color java.awt.Color/red :series 1)
      (set-stroke-color java.awt.Color/blue :series 2)
      (set-stroke-color java.awt.Color/green :series 3)
      (save fname :width 300 :height 250)
      ))
  :ok)

(defn plot-one-res-square [flag line neg-points pos-points base res-line]
  (if (= flag true) (plot-one-res-square-aux line
                                             neg-points
                                             pos-points
                                             base res-line)))

(defn mk-rand []
  (- (rand 2.000001) 1))

(defn mk-point []
  [(mk-rand)
   (mk-rand)]
  )

(defn mk-any-line []
  [(mk-point)
   (mk-point)]
  )

(defn calc-line-w [ [[x1 y1 :as p1] [x2 y2 :as p2]] ]
  (let [dy (- y2 y1)
        dx (- x2 x1)
        c (+ (* (- x1) dy)
             (* y1 dx))]
    [dy (- dx) c]))

(defn normalize-by-x [kx ky c]
  (let [
        x1 -1
        y1 (/ (- kx c) ky)
        x2 1
        y2 (/ (- (+ kx c)) ky)
        ]
    [[x1 y1] [x2 y2]]
    )
  )

(defn normalize-by-y [kx ky c]
  (let [
        y1 -1
        x1 (/ (- ky c) kx)
        y2 1
        x2 (/ (- (+ ky c)) kx)
        ]
    [[x1 y1] [x2 y2]]
    )
  )

;; given line coefficients, return two points located far enough from
;; each other
(defn normalize [kx ky c]
  (cond
    (and (= kx 0.0) (= ky 0.0)) (assert false "both kx and ky are zero")
    (= kx 0.0) (normalize-by-x kx ky c)
    (= ky 0.0) (normalize-by-y kx ky c)
    (< (nl-trans.misc/abs kx) (nl-trans.misc/abs ky)) (normalize-by-x kx ky c)
    :default (normalize-by-y kx ky c)))

(defn mk-norm-line []
  (let [line (mk-any-line)
        [kx ky c] (calc-line-w line)]
    (normalize kx ky c)))

(defn mk-line []
  (mk-norm-line))

(defn gen-points [n]
  (repeatedly n mk-point))

(defn calc-f [[x1 x2]]
  (let [val (+ (* x1 x1)
               (* x2 x2)
               (- 0.6))]
    (if (>= val 0) 1
        -1)))

(defn calc-f-nl [[x1 x2]
                 [w0 w1 w2 w3 w4 w5]]
  (let [val (+ (* w0 1)
               (* w1 x1)
               (* w2 x2)
               (* w3 x1 x2)
               (* w4 x1 x1)
               (* w5 x2 x2)
               )]
    (if (>= val 0) 1
        -1)))

(defn flip-sign [limit y]
  (if (< (rand) limit) (- y)
      y))

(defn gen-noised-ys [ys]
  (map #(flip-sign 0.1 %) ys))

(defn calc-one-y2 [[w0 w1 w2]
                   [x1 x2]]
  (let [p (+ (* w0 1)
             (* w1 x1)
             (* w2 x2))]
    (if (>= p 0) 1
        -1)))

(defn calc-one-y [
                  [[x0 y0] [x1 y1]]
                  [x y]
                  ]
  (let [
        dy (- y1 y0)
        dx (- x1 x0)
        res (+ (* x dy)
               (* (- y) dx)
               (* (- x0) dy)
               (* y0 dx)
               )
        ]
    (if (>= res 0) 1
        -1)))

(defn calc-y [line points]
  (map #(calc-one-y line %) points))

(defn split-points [ys points]
  (let [merged (map list ys points)
        {neg-merged false, pos-merged true} (group-by #(pos? (first %)) merged)
        neg (map second neg-merged)
        pos (map second pos-merged)]
    [neg pos]))

;; calc pseudo-inverse of a matrix: (X^t * X)^-1 * X^t
(defn pseudo-inverse [x]
  (let [xt (incanter.core/trans x)
        prod (incanter.core/mmult xt x)]
    (incanter.core/solve prod xt)))

(defn reg-pure [ys points]
  (let [b-points (map #(cons 1 %) points)
        matrix (incanter.core/matrix b-points)
        dagger (pseudo-inverse matrix)
        y-m (incanter.core/matrix ys)
        w (incanter.core/mmult dagger y-m)]
    (incanter.core/to-vect w)))

(defn reg-aux [i ys points]
  (let [_ (if (> i 0) (nl-trans.misc/log-val "reg-aux, i" i))
        _ (if (> i 10) (nl-trans.misc/log-val "reg-aux, i" i "ys" "points" points))
        [_w0 w1 w2 :as w] (reg-pure ys points)]
    (if (and (= w1 0.0)
             (= w2 0.0)
             (< i 11)) (recur (inc i) ys points)
        w)))

(defn reg [ys points]
  (reg-aux 0 ys points))

(defn is-misclassified [[w0 w1 w2 :as w]
                        [y
                         [x1 x2 :as point]]]
  (if (= w [0 0 0]) true
      (let [p (calc-one-y2 w point)]
        (not (= p y)))))

(defn get-misclassified [w ys points]
  (let [merged (map list ys points)
        mis-all (filter #(is-misclassified w %) merged)]
    (first (shuffle mis-all))))

(defn update-w [[w0 w1 w2]
                y
                [x1 x2]]
  [(+ w0 (* y 1))
   (+ w1 (* y x1))
   (+ w2 (* y x2))])

(defn pla-aux [i w ys points]
  (let [[mis-y mis-point :as mis] (get-misclassified w ys points)
        _ (nl-trans.misc/log-val "pla-aux" "i" i "w" w "mis" mis)
        ]
    (if (nil? mis) [i w]
        (let [new-w (update-w w mis-y mis-point)]
          (recur (inc i) new-w ys points)))))

(defn pla [w ys points]
  (pla-aux 0 w ys points))

(defn line-outside [line]
  (let [points [[-1 -1]
                [-1 1]
                [1 -1]
                [1 1]]
        y (map #(calc-one-y line %) points)
        y1 (filter #(= -1 %) y)
        y2 (filter #(= 1 %) y)]
    (if (or (empty? y1) (empty? y2)) true
        false)))

(defn calc-diff-prob-aux [i acc line res-line]
  (if (= i 0) acc
      (let [point (mk-point)
            y0 (calc-one-y line point)
            y1 (calc-one-y res-line point)]
        (if (= y0 y1) (recur (dec i) acc line res-line)
            (recur (dec i) (inc acc) line res-line)))))

(defn calc-diff-prob [line res-line]
  (if (line-outside res-line) 1
      (let [n 10000
            delta (calc-diff-prob-aux n 0 line res-line)]
        (float (/ delta n)))))

(defn mismatched-y [[y0 y1]]
  (not (= y0 y1)))

(defn e-aux [orig-line res-line points]
  (let [ys0 (calc-y orig-line points)
        ys1 (calc-y res-line points)
        joined (map #(vec [%1 %2]) ys0 ys1)
        mismatched (filter mismatched-y joined)
        n (count mismatched)
        total (count points)]
    (float (/ n total))))

(defn e-in [orig-line res-line points]
  (e-aux orig-line res-line points))

(defn e-out [orig-line res-line n0]
  (let [n (* n0 10)
        points (gen-points n)]
    (e-aux orig-line res-line points)))

(defn log-zero-w [[w0 w1 w2] line ys points]
  (cond
    (not (= w1 0.0)) nil
    (not (= w2 0.0)) nil
    :default (println "kx and ky are zero"
                      "\nline:" line
                      "\nys" ys
                      "\npoints" points)
    )
  )

(defn calc-one-step-aux [n pic base]
  (let [line (mk-line)
        points (gen-points n)
        ys-pure (map calc-f points)
        ys-noise (gen-noised-ys ys-pure)
        ys ys-noise
        [neg-points pos-points] (split-points ys points)
        [wr0 wr1 wr2 :as res-w] (reg ys points)
        _ (nl-trans.misc/log-val "res-w" res-w)
        _ (log-zero-w res-w line ys points)
        res-line (normalize wr1 wr2 wr0)
        base-reg (apply str [base "-reg"])
        _ (plot-one-res-square pic line neg-points pos-points base-reg res-line)
        ein (e-in line res-line points)
        _ (nl-trans.misc/log-val "e-in" ein)
        ;; eout (e-out line res-line n)
        ;; _ (nl-trans.misc/log-val "e-out" eout)
        diff-p (calc-diff-prob line res-line)
        _ (nl-trans.misc/log-val "diff p" diff-p)
        ;; [pla-iters [wp0 wp1 wp2] :as pla-res] (pla res-w ys points)
        ;; _ (nl-trans.misc/log-val "pla res" pla-res)
        ;; res-line-pla (normalize wp1 wp2 wp0)
        ;; base-pla (apply str [base "-pla"])
        ;; _ (plot-one-res-square pic line neg-points pos-points
        ;;                        base-pla
        ;;                        res-line-pla)
        ]
    ;; [ein eout diff-p pla-iters]
    [ein diff-p]
    )
  )

(defn calc-one-step [n pic base0 i]
  (let [base (apply str [base0 "-" i])]
    (calc-one-step-aux n pic base)))

(defn calc [n cnt pic]
  (let [base (int (rand 1000))
        _ (nl-trans.misc/log-val "n" n "cnt" cnt "base" base)
        res (for [i (range cnt)]
              (calc-one-step n pic base i))
        _ (nl-trans.misc/log-val "all step res" res)
        sum-e-in (reduce + (map first res))
        ;; sum-e-out (reduce + (map second res))
        sum-probs (reduce + (map #(get % 1) res)) ;; 
        ;; sum-iters (reduce + (map #(get % 3) res))
        avg-e-in (float (/ sum-e-in cnt))
        ;; avg-e-out (float (/ sum-e-out cnt))
        avg-probs (float (/ sum-probs cnt))
        ;; avg-iters (float (/ sum-iters cnt))
        ]
    ;; [avg-e-in avg-e-out avg-probs avg-iters]
    [avg-e-in avg-probs]
    )
  )

