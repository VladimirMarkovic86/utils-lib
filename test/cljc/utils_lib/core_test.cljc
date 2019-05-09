(ns utils-lib.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [utils-lib.core :refer [round-up round-decimals remove-index-from-vector
                                    replace-in-vector-on-index insert-in-vector-on-index
                                    sha256 split-with-newline is-number? nth-root
                                    calculate-circle-coordinates
                                    get-quadrate-extreme-points find-quadrate]]))

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

(deftest test-nth-root
  (testing "Test nth root"
    
    (let [base nil
          n nil
          result (nth-root
                   base
                   n)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [base 4
          n 2
          result (nth-root
                   base
                   n)]
      
      (is
        (= result
           1.9999999999999998)
       )
      
     )
    
    (let [base 8
          n 3
          result (nth-root
                   base
                   n)]
      
      (is
        (= result
           1.9999999999999998)
       )
      
     )
    
    (let [base 25
          n 2
          result (nth-root
                   base
                   n)]
      
      (is
        (= result
           4.999999999999999)
       )
      
     )
    
    (let [base 125
          n 3
          result (nth-root
                   base
                   n)]
      
      (is
        (= result
           4.9999999999999998)
       )
      
     )
    
   ))

(deftest test-calculate-circle-coordinates
  (testing "Test calculate circle coordinates"
    
    (let [r nil
          angle nil
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [r 200
          angle 0
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (= result
           [200 0])
       )
      
     )
    
    (let [r 200
          angle 45
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (= result
           [141 141])
       )
      
     )
    
    (let [r 200
          angle 90
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (= result
           [0 200])
       )
      
     )
    
    (let [r 200
          angle 135
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (= result
           [-141 141])
       )
      
     )
    
    (let [r 200
          angle 180
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (= result
           [-200 0])
       )
      
     )
    
    (let [r 200
          angle 225
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (= result
           [-141 -141])
       )
      
     )
    
    (let [r 200
          angle 270
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (= result
           [0 -200])
       )
      
     )
    
    (let [r 200
          angle 315
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (= result
           [141 -141])
       )
      
     )
    
    (let [r 200
          angle 360
          result (calculate-circle-coordinates
                   r
                   angle)]
      
      (is
        (= result
           [200 0])
       )
      
     )
    
   ))

(deftest test-get-quadrate-extreme-points
  (testing "Test get quadrate extreme points"
    
    (let [quadrate-number nil
          radius nil
          result (get-quadrate-extreme-points
                   quadrate-number
                   radius)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [quadrate-number 0
          radius 200
          result (get-quadrate-extreme-points
                   quadrate-number
                   radius)]
      
      (is
        (= result
           {:start [200 0]
            :angle [200 200]
            :end [0 200]})
       )
      
     )
    
    (let [quadrate-number 1
          radius 200
          result (get-quadrate-extreme-points
                   quadrate-number
                   radius)]
      
      (is
        (= result
           {:start [0 200]
            :angle [(- 200) 200]
            :end [(- 200) 0]})
       )
      
     )
    
    (let [quadrate-number 2
          radius 200
          result (get-quadrate-extreme-points
                   quadrate-number
                   radius)]
      
      (is
        (= result
           {:start [(- 200) 0]
            :angle [(- 200) (- 200)]
            :end [0 (- 200)]})
       )
      
     )
    
    (let [quadrate-number 3
          radius 200
          result (get-quadrate-extreme-points
                   quadrate-number
                   radius)]
      
      (is
        (= result
           {:start [0 (- 200)]
            :angle [200 (- 200)]
            :end [200 0]})
       )
      
     )
    
   ))

(deftest test-find-quadrate
  (testing "Test find quadrate"
    
    (let [x nil
          y nil
          result (find-quadrate
                   x
                   y)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [x 0
          y 0
          result (find-quadrate
                   x
                   y)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [x 1
          y 0
          result (find-quadrate
                   x
                   y)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [x 1
          y 1
          result (find-quadrate
                   x
                   y)]
      
      (is
        (= result
           0)
       )
      
     )
    
    (let [x -1
          y 0
          result (find-quadrate
                   x
                   y)]
      
      (is
        (= result
           1)
       )
      
     )
    
    (let [x -1
          y 1
          result (find-quadrate
                   x
                   y)]
      
      (is
        (= result
           1)
       )
      
     )
    
    (let [x -1
          y -1
          result (find-quadrate
                   x
                   y)]
      
      (is
        (= result
           2)
       )
      
     )
    
    (let [x 0
          y -1
          result (find-quadrate
                   x
                   y)]
      
      (is
        (= result
           3)
       )
      
     )
    
    (let [x 1
          y -1
          result (find-quadrate
                   x
                   y)]
      
      (is
        (= result
           3)
       )
      
     )
    
   ))

