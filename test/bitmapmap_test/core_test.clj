(ns bitmapmap-test.core-test
  (:use clojure.test))

(defmacro time-taken
  "Evaluates expr and prints and returns the time it took, in msec."
  [expr]
  `(let [start#   (System/nanoTime)
         _#       ~expr
         time-ms# (/ (double (- (System/nanoTime) start#)) 1000000.0)]
     (prn (str "Elapsed time: " time-ms# " msecs"))
     time-ms#))

(defn test-get [k]
  (fn [name m ntimes]
    (print \tab name \tab)
    (time-taken (dotimes [_ ntimes]
                  (get m k)))))

(defn test-assoc [k]
  (fn [name m ntimes]
    (print \tab name \tab)
    (time-taken (dotimes [_ ntimes]
                  (assoc m k "42")))))

(defn test-maps [name args testfn]
  (let [am (apply array-map args)
        bm (apply bitmap-map args)
        hm (apply hash-map args)
        testrun (fn [n am bm hm N]
                  (println (str "  Run #" (inc n)))
                  (dotimes [_ 10] (testfn "array-map" am N))
                  (dotimes [_ 10] (testfn "bitmap-map" bm N))
                  (dotimes [_ 10] (testfn "hashmap-map" hm N)))
        N 2e6]
    (println "Testing maps of size" (.size am) "with" name)
    (dotimes [n 3]
      (testrun n am bm hm N))
    (let [am-time (testfn "array-map" am N)
          bm-time (testfn "bitmap-map" bm N)
          hm-time (testfn "hashmap" hm N)
          ]
      (println "array-map/hashmap" (/ am-time hm-time))
      (println "bitmap-map/hashmap" (/ bm-time hm-time))
      (println "bitmap/arraymap" (/ bm-time am-time)))))

(defn mapargs [size]
  (mapcat (juxt (comp vector str) identity)
          (range size)))

(deftest get-test
  (test-maps "get with nonidentical key" (mapargs 3) (test-get (vector "2")))
  (test-maps "get with nonidentical key" (mapargs 4) (test-get (vector "3")))
  (test-maps "get with nonidentical key" (mapargs 5) (test-get (vector "4")))
  (doseq [size [3 4 5]]
    (let [args (mapargs size)
          key  (last (butlast args))]
      (test-maps "get with identical key" args (test-get key)))))

(deftest assoc-existing-key-test
  (test-maps "assoc with nonidentical key" (mapargs 3) (test-assoc (vector "2")))
  (test-maps "assoc with nonidentical key" (mapargs 4) (test-assoc (vector "3")))
  (test-maps "assoc with nonidentical key" (mapargs 5) (test-assoc (vector "4")))
  (doseq [size [3 4 5]]
    (let [args (mapargs size)
          key  (last (butlast args))]
      (test-maps "assoc with identical key" args (test-assoc key)))))

(deftest assoc-new-key-test
  (test-maps "assoc with new key" (mapargs 3) (test-assoc (vector "foo")))
  (test-maps "assoc with new key" (mapargs 4) (test-assoc (vector "foo")))
  (test-maps "assoc with new key" (mapargs 5) (test-assoc (vector "foo"))))
