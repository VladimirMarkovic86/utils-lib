(ns utils-lib.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [utils-lib.core :refer [round-up round-decimals remove-index-from-vector
                                    replace-in-vector-on-index insert-in-vector-on-index
                                    sha256 split-with-newline is-number? nth-root
                                    calculate-circle-coordinates
                                    get-quadrate-extreme-points find-quadrate
                                    find-quadrate-by-angle calculate-angle
                                    calculate-angle-by-coordinates-asin
                                    calculate-angle-by-coordinates-acos
                                    calculate-radius-by-x calculate-radius-by-y
                                    rotate-x rotate-y rotate-coordinates decode-ascii
                                    format-bytes-number
                                    format-bytes-number-with-lowest-number-of-digits]]))

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
            :end [0 200]
            :start-angle 0
            :end-angle 90})
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
            :end [(- 200) 0]
            :start-angle 90
            :end-angle 180})
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
            :end [0 (- 200)]
            :start-angle 180
            :end-angle 270})
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
            :end [200 0]
            :start-angle 270
            :end-angle 360})
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

(deftest test-find-quadrate-by-angle
  (testing "Test find quadrate by angle"
    
    (let [start-angle nil
          end-angle nil
          result (find-quadrate-by-angle
                   start-angle
                   end-angle)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [start-angle 0
          end-angle 90
          result (find-quadrate-by-angle
                   start-angle
                   end-angle)]
      
      (is
        (= result
           [0])
       )
      
     )
    
    (let [start-angle 90
          end-angle 180
          result (find-quadrate-by-angle
                   start-angle
                   end-angle)]
      
      (is
        (= result
           [1])
       )
      
     )
    
    (let [start-angle 180
          end-angle 270
          result (find-quadrate-by-angle
                   start-angle
                   end-angle)]
      
      (is
        (= result
           [2])
       )
      
     )
    
    (let [start-angle 270
          end-angle 360
          result (find-quadrate-by-angle
                   start-angle
                   end-angle)]
      
      (is
        (= result
           [3])
       )
      
     )
    
    (let [start-angle 0
          end-angle 360
          result (find-quadrate-by-angle
                   start-angle
                   end-angle)]
      
      (is
        (= result
           [0 1 2 3])
       )
      
     )
    
    (let [start-angle 30
          end-angle 150
          result (find-quadrate-by-angle
                   start-angle
                   end-angle)]
      
      (is
        (= result
           [0 1])
       )
      
     )
    
    (let [start-angle 200
          end-angle 290
          result (find-quadrate-by-angle
                   start-angle
                   end-angle)]
      
      (is
        (= result
           [2 3])
       )
      
     )
    
   ))

(deftest test-calculate-angle
  (testing "Test calculate angle"
    
    (let [angle-degrees nil
          result (calculate-angle
                   angle-degrees)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [angle-degrees 0
          result (calculate-angle
                   angle-degrees)]
      
      (is
        (= result
           6.283185307179586)
       )
      
     )
    
    (let [angle-degrees 90
          result (calculate-angle
                   angle-degrees)]
      
      (is
        (= result
           4.71238898038469)
       )
      
     )
    
    (let [angle-degrees 180
          result (calculate-angle
                   angle-degrees)]
      
      (is
        (= result
           3.141592653589793)
       )
      
     )
    
    (let [angle-degrees 270
          result (calculate-angle
                   angle-degrees)]
      
      (is
        (= result
           1.5707963267948966)
       )
      
     )
    
    (let [angle-degrees 360
          result (calculate-angle
                   angle-degrees)]
      
      (is
        (= result
           6.283185307179586)
       )
      
     )
    
   ))

(deftest test-calculate-angle-by-coordinates-asin
  (testing "Test calculate angle by coordinates asin"
    
    (let [x nil
          y nil
          result (calculate-angle-by-coordinates-asin
                   x
                   y)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [x 0
          y 1
          result (calculate-angle-by-coordinates-asin
                   x
                   y)]
      
      (is
        (= result
           90.0)
       )
      
     )
    
    (let [x 1
          y 1
          result (calculate-angle-by-coordinates-asin
                   x
                   y)]
      
      (is
        (= result
           45.00000000000001)
       )
      
     )
    
    (let [x -1
          y 1
          result (calculate-angle-by-coordinates-asin
                   x
                   y)]
      
      (is
        (= result
           45.00000000000001)
       )
      
     )
    
   ))

(deftest test-calculate-angle-by-coordinates-acos
  (testing "Test calculate angle by coordinates acos"
    
    (let [x nil
          y nil
          result (calculate-angle-by-coordinates-acos
                   x
                   y)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [x 1
          y 0
          result (calculate-angle-by-coordinates-acos
                   x
                   y)]
      
      (is
        (= result
           0.0)
       )
      
     )
    
    (let [x 1
          y 1
          result (calculate-angle-by-coordinates-acos
                   x
                   y)]
      
      (is
        (= result
           45.0)
       )
      
     )
    
    (let [x 1
          y -1
          result (calculate-angle-by-coordinates-acos
                   x
                   y)]
      
      (is
        (= result
           45.0)
       )
      
     )
    
   ))

(deftest test-calculate-radius-by-x
  (testing "Test calculate radius by x"
    
    (let [x nil
          angle nil
          result (calculate-radius-by-x
                   x
                   angle)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [x 0
          angle 90
          result (calculate-radius-by-x
                   x
                   angle)]
      
      (is
        (= result
           0.0)
       )
      
     )
    
    (let [x 0
          angle 0
          result (calculate-radius-by-x
                   x
                   angle)]
      
      (is
        (= result
           0.0)
       )
      
     )
    
    (let [x 10
          angle 20
          result (calculate-radius-by-x
                   x
                   angle)]
      
      (is
        (= result
           10.641777724759121)
       )
      
     )
    
    (let [x 50
          angle 20
          result (calculate-radius-by-x
                   x
                   angle)]
      
      (is
        (= result
           53.2088886237956)
       )
      
     )
    
   ))

(deftest test-calculate-radius-by-y
  (testing "Test calculate radius by y"
    
    (let [y nil
          angle nil
          result (calculate-radius-by-y
                   y
                   angle)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [y 0
          angle 0
          result (calculate-radius-by-y
                   y
                   angle)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [y 0
          angle 90
          result (calculate-radius-by-y
                   y
                   angle)]
      
      (is
        (= result
           0.0)
       )
      
     )
    
    (let [y 10
          angle 20
          result (calculate-radius-by-y
                   y
                   angle)]
      
      (is
        (= result
           29.238044001630875)
       )
      
     )
    
    (let [y 50
          angle 20
          result (calculate-radius-by-y
                   y
                   angle)]
      
      (is
        (= result
           146.19022000815437)
       )
      
     )
    
   ))

(deftest test-rotate-x
  (testing "Test rotate x"
    
    (let [r nil
          angle nil
          result (rotate-x
                   r
                   angle)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [r 100
          angle 20
          result (rotate-x
                   r
                   angle)]
      
      (is
        (= result
           93.96926207859084)
       )
      
     )
    
    (let [r 100
          angle 70
          result (rotate-x
                   r
                   angle)]
      
      (is
        (= result
           34.20201433256688)
       )
      
     )
    
   ))

(deftest test-rotate-y
  (testing "Test rotate y"
    
    (let [r nil
          angle nil
          result (rotate-y
                   r
                   angle)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [r 100
          angle 20
          result (rotate-y
                   r
                   angle)]
      
      (is
        (= result
           34.20201433256687)
       )
      
     )
    
    (let [r 100
          angle 70
          result (rotate-y
                   r
                   angle)]
      
      (is
        (= result
           93.96926207859083)
       )
      
     )
    
   ))

(deftest test-rotate-coordinates
  (testing "Test rotate coordinates"
    
    (let [x nil
          y nil
          angle nil
          result (rotate-coordinates
                   x
                   y
                   angle)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [x 100
          y 0
          angle 45
          result (rotate-coordinates
                   x
                   y
                   angle)]
      
      (is
        (= result
           [70.71067811865476
            70.71067811865474])
       )
      
     )
    
    (let [x 50
          y 20
          angle 90
          result (rotate-coordinates
                   x
                   y
                   angle)]
      
      (is
        (= result
           [-20.000000000000007
            50.0])
       )
      
     )
    
   ))

(deftest test-decode-ascii
  (testing "Test decode ascii"
    
    (let [text nil
          result (decode-ascii
                   text)]
      
      (is
        (string?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [text ""
          result (decode-ascii
                   text)]
      
      (is
        (string?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [text "test%20test"
          result (decode-ascii
                   text)]
      
      (is
        (string?
          result)
       )
      
      (is
        (= result
           "test test")
       )
      
     )
    
   ))

(deftest test-format-bytes-number
  (testing "Test format bytes number"
    
    (let [number-of-bytes nil
          unit-type nil
          result (format-bytes-number
                   number-of-bytes
                   unit-type)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [number-of-bytes 123
          unit-type nil
          result (format-bytes-number
                   number-of-bytes
                   unit-type)]
      
      (is
        (= result
           "123.0 B")
       )
      
     )
    
    (let [number-of-bytes 1024
          unit-type "KB"
          result (format-bytes-number
                   number-of-bytes
                   unit-type)]
      
      (is
        (= result
           "1.0 KB")
       )
      
     )
    
    (let [number-of-bytes 4000
          unit-type "KB"
          result (format-bytes-number
                   number-of-bytes
                   unit-type)]
      
      (is
        (= result
           "3.91 KB")
       )
      
     )
    
    (let [number-of-bytes 4000000
          unit-type "MB"
          result (format-bytes-number
                   number-of-bytes
                   unit-type)]
      
      (is
        (= result
           "3.81 MB")
       )
      
     )
    
   ))

(deftest test-format-bytes-number-with-lowest-number-of-digits
  (testing "Test format bytes number with lowest number of digits"
    
    (let [number-of-bytes nil
          result (format-bytes-number-with-lowest-number-of-digits
                   number-of-bytes)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [number-of-bytes 1024
          result (format-bytes-number-with-lowest-number-of-digits
                   number-of-bytes)]
      
      (is
        (= result
           "1.0 KB")
       )
      
     )
    
    (let [number-of-bytes 4000
          result (format-bytes-number-with-lowest-number-of-digits
                   number-of-bytes)]
      
      (is
        (= result
           "3.91 KB")
       )
      
     )
    
    (let [number-of-bytes 4000000
          result (format-bytes-number-with-lowest-number-of-digits
                   number-of-bytes)]
      
      (is
        (= result
           "3.81 MB")
       )
      
     )
    
   ))

