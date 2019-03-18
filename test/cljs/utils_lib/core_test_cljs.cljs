(ns utils-lib.core-test-cljs
  (:require [clojure.test :refer-macros [deftest is testing]]
            [utils-lib.core :refer [current-date]]))

(deftest test-current-date
  (testing "Test current date"
    
    (let [string-date (current-date)
          current-date-obj (js/Date.)]
      
      (is
        (= js/String
           (type
             string-date))
       )
    
      (is
        (= js/Date
           (type
             (js/Date.
               string-date))
         ))
      
      (is
        (< (- (.getTime
                current-date-obj)
              (.getTime
                (js/Date.
                  string-date)))
           (* 1000
              60))
       )
      
     )
    
   ))

