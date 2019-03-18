(ns utils-lib.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [utils-lib.core :refer [round-up round-decimals remove-index-from-vector
                                    replace-in-vector-on-index insert-in-vector-on-index
                                    sha256 split-with-newline is-number?]]))

(deftest test-round-up
  (testing "Test round up function"
  
    (is
      (= (round-up
           1
           3)
         1))
    
    (is
      (= (round-up
           3
           3)
         1))
    
    (is
      (= (round-up
           4
           3)
         2))
    
    (is
      (= (round-up
           6
           3)
         2))
    
    (is
      (= (round-up
           7
           3)
         3))
    
    (is
      (= (round-up
           9
           3)
         3))
    
   ))

(deftest test-round-decimals
  (testing "Test round to a particular number of decimals"
  
    (is (= (round-decimals
             1.23456
             1)
           1.2))
    
    (is (= (round-decimals
             1.23456
             2)
           1.23))
    
    (is (= (round-decimals
             1.23456
             3)
           1.235))
    
    (is (= (round-decimals
             1.234
             3)
           1.234))
    
    (is (= (round-decimals
             1.23
             3)
           1.23))
    
    (is (= (round-decimals
             1.0
             3)
           1.0))
    
    (is (= (round-decimals
             1
             3)
           1.0))
    
    (is (= (round-decimals
             1.0
             0)
           1))
    
    (is (= (round-decimals
             1
             0)
           1))
    
    (is (= (round-decimals
             1
             -1)
           1))
    
   ))

(deftest test-remove-index-from-vector
  (testing "Test remove index from vector"
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           1)
         [1 3]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           2)
         [1 2]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           0)
         [2 3]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           3)
         [1 2 3]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           4)
         [1 2 3]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           -1)
         [1 2 3]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           [0 1])
         [3]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           [0 2])
         [2]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           [-1 2])
         [1 2]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           [0 3])
         [2 3]))
  
    (is
      (= (remove-index-from-vector
           [1 2 3]
           [-1 3])
         [1 2 3]))
    
   ))

(deftest test-replace-in-vector-on-index
  (testing "Test replace in vector on index"
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           22
           1)
         [1 22 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           33
           2)
         [1 2 33]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           11
           0)
         [11 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           44
           3)
         [1 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           55
           4)
         [1 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           -11
           -1)
         [1 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [11 22]
           [0 1])
         [11 22 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [11 33]
           [0 2])
         [11 2 33]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [-11 33]
           [-1 2])
         [1 2 33]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [-22 -11 33]
           [-2 -1 2])
         [1 2 33]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [-22 -11 11]
           [-2 -1 0])
         [11 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [-22 -11 22]
           [-2 -1 1])
         [1 22 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [11 44]
           [0 3])
         [11 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [-11 44]
           [-1 3])
         [1 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [-11]
           [-1 2])
         [1 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [-11]
           [-1 2 3 4 5])
         [1 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [-11 11]
           [-1])
         [1 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [-11 11]
           [-1 0 1])
         [11 2 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [22]
           [1 2])
         [1 22 3]))
  
    (is
      (= (replace-in-vector-on-index
           [1 2 3]
           [22 33]
           [1])
         [1 22 3]))
    (let [new-elem (atom
                     22)]
      (is
        (= (replace-in-vector-on-index
             [1 2 3]
             new-elem
             1)
           [1 new-elem 3]))
     )
    
    
   ))

(deftest test-insert-in-vector-on-index
  (testing "Test insert in vector on index"
  
    (is
      (= (insert-in-vector-on-index
           [1 2 3]
           12
           1)
         [1 12 2 3]))
  
    (is
      (= (insert-in-vector-on-index
           [1 2 3]
           23
           2)
         [1 2 23 3]))
  
    (is
      (= (insert-in-vector-on-index
           [1 2 3]
           -11
           -1)
         [1 2 3]))
  
    (is
      (= (insert-in-vector-on-index
           [1 2 3]
           [12 23]
           [1 2])
         [1 12 2 23 3]))
  
    (is
      (= (insert-in-vector-on-index
           [1 2 3]
           [11 12]
           [0 1 2])
         [11 1 12 2 3]))
  
    (is
      (= (insert-in-vector-on-index
           [1 2 3]
           [11 12]
           [0])
         [11 1 2 3]))
  
    (is
      (= (insert-in-vector-on-index
           [1 2 3]
           [-11 11]
           [-1 0])
         [11 1 2 3]))
  
    (is
      (= (insert-in-vector-on-index
           [1]
           [22]
           [0])
         [22 1]))
  
    (is
      (= (insert-in-vector-on-index
           [1]
           [22]
           [1])
         [1]))
    
   ))

(deftest test-sha256
  (testing "Test sha256"
  
    (is
      (= (sha256
           "abcdefg")
         "7d1a54127b222502f5b79b5fb0803061152a44f92b37e23c6527baf665d4da9a"))

   ))

(deftest test-split-with-newline
  (testing "Test split with newline"
  
    (is
      (= (split-with-newline
           "test\ntest2")
         ["test\n" "test2"]))
  
    (is
      (= (split-with-newline
           "test\n\ntest2")
         ["test\n" "\n" "test2"]))
  
    (is
      (= (split-with-newline
           "test\ntest1\ntest2")
         ["test\n" "test1\n" "test2"]))
  
    (is
      (= (split-with-newline
           "\n\ntest\ntest1\ntest2")
         ["\n" "\n" "test\n" "test1\n" "test2"]))
  
    (is
      (= (split-with-newline
           "test\ntest1\ntest2\n\n\n")
         ["test\n" "test1\n" "test2\n" "\n" "\n"]))
  
    (is
      (= (split-with-newline
           "\n\ntest\ntest1\ntest2\n\n\n")
         ["\n" "\n" "test\n" "test1\n" "test2\n" "\n" "\n"]))

   ))

(deftest test-is-number
  (testing "Test if parameter is number"
  
    (is
      (is-number?
        1))
  
    (is
      (not
        (is-number?
          "1"))
     )
    
   ))

