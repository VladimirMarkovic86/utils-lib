(ns utils-lib.core-test-clj
  (:require [clojure.test :refer :all]
            [utils-lib.core :refer :all]))

(deftest test-java-heap-size
  (testing "Test java heap size function"
    
    (let [fn-result (java-heap-size)]
      (is
        (and (contains?
               fn-result
               :used-memory)
             (contains?
               fn-result
               :free-memory)
             (contains?
               fn-result
               :total-memory)
             (contains?
               fn-result
               :max-memory))
       ))
    
   ))

(deftest test-current-date
  (testing "Test current date"
    
    (let [sdf (java.text.SimpleDateFormat.
                "E, dd MMM yyyy, HH:mm:ss zzz")
          string-date (current-date)
          current-date-obj (java.util.Date.)]
      
      (is
        (< (- (.getTime
                current-date-obj)
              (.getTime
                (.parse
                  sdf
                  string-date)))
           (* 1000
              60))
       )
      
      (is
        (instance?
          java.util.Date
          (.parse
            sdf
            string-date))
       )
      
     )

   ))

