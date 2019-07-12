(ns utils-lib.core-test-clj
  (:require [clojure.test :refer :all]
            [utils-lib.core :refer :all]
            [utils-lib.core-clj :refer :all]
            [clojure.string :as cstring]))

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

(deftest test-java-available-processors
  (testing "Test java available processors"
    
    (let [result (java-available-processors)]
      
      (is
        (number?
          result)
       )
      
      (is
        (< 0
           result)
       )
      
     )
    
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

(deftest test-sh-exists?
  (testing "Test if sh file exists"
    
    (is
      (instance?
        java.lang.Boolean
        (sh-exists?))
     )
    
   ))

(deftest test-make-sh-file
  (testing "Test make sh file"
    
    (make-sh-file)
    
    (is
      (sh-exists?)
     )
    
   ))

(deftest test-execute-shell-command
  (testing "Test execute shell command"
    
    (let [result (execute-shell-command
                   "ls /")
          result-out (:out result)]
      
      (is
        (not
          (nil?
            (cstring/index-of
              result-out
              "home"))
         )
       )
      
     )
    
   ))

(deftest test-get-lan-ip4-address
  (testing "Test get lan ip4 address"
    
    (let [result (get-lan-ip4-address)]
      
      (is
        (cstring/index-of
          result
          "192.168.1.")
       )
      
     )
    
   ))

