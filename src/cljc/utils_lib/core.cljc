(ns utils-lib.core
  (:require [clojure.string :as cstring]
            [utils-lib.constants :as const]))

(defn round-up
  "Round up divided numbers"
  [number1
   number2]
  (if (= 0
         (mod
           number1
           number2))
   (int
     (/ number1
        number2))
   (inc
     (int
       (/ number1
          number2))
    ))
 )

(defn round-decimals
  "Round number to particular number of decimals"
  [number
   decimals-num]
  #?(:clj
      (let [divider (atom 1)
            final-number (atom nil)]
        (doseq [itr (range
                      decimals-num)]
          (swap!
            divider
            *
            10))
        (reset!
          final-number
          (/ (Math/round
               (* (double
                    number)
                  @divider))
             @divider))
        (if (= @divider
               1)
          (int
            @final-number)
          (double
            @final-number))
       )
     :cljs
      (let [divider (atom 1)
            final-number (atom nil)]
        (doseq [itr (range
                      decimals-num)]
          (swap!
            divider
            *
            10))
        (reset!
          final-number
          (/ (.round
               js/Math
               (* (double
                    number)
                  @divider))
             @divider))
        (if (= @divider
               1)
          (int
            @final-number)
          (double
            @final-number))
       ))
 )

(defn remove-index-from-vector
  "Remove data at particular index or indexes
   
   data-vector is simple vector with elements of any data type
   index is single number or vector of numbers that represent index/es
   that should be removed"
  [data-vector
   index]
  (let [indexes-to-remove (if (number?
                                index)
                            #{index}
                            (when (or (seq?
                                        index)
                                      (vector?
                                        index)
                                      (set?
                                        index))
                              (into
                                #{}
                                index))
                           )
        result (atom [])]
    (dotimes [i (count
                  data-vector)]
      (when-not (contains?
                  indexes-to-remove
                  i)
        (swap!
          result
          conj
          (get
            data-vector
            i))
       ))
    @result))

(defn replace-in-vector-on-index
  "Replace data in vector on particular indexes
   
   data-vector is vector with elements of any data type
   element is single element or vector of elements that are going to replace old data
   index is single number or vector of numbers that represent positions in data vector
    where replacement/s will take place"
  [data-vector
   element
   index]
  (let [[new-elements
         indexes-to-replace] (if (number?
                                   index)
                               [(atom
                                  [element])
                                (atom
                                  [index])]
                               (when (and (vector?
                                            element)
                                          (vector?
                                            index))
                                 [(atom
                                    element)
                                  (atom
                                    index)])
                              )
        remove-first-fn (fn [param]
                          (into
                            []
                            (rest
                              param))
                         )
        result (atom [])]
    (when (< (count
               @new-elements)
             (count
               @indexes-to-replace))
      (let [new-indexes (atom [])]
        (dotimes [i (count
                      @new-elements)]
          (swap!
            new-indexes
            conj
            (get
              @indexes-to-replace
              i))
         )
        (reset!
          indexes-to-replace
          @new-indexes))
     )
    (when (< (count
               @indexes-to-replace)
             (count
               @new-elements))
      (let [new-elements-a (atom [])]
        (dotimes [i (count
                      @indexes-to-replace)]
          (swap!
            new-elements-a
            conj
            (get
              @new-elements
              i))
         )
        (reset!
          new-elements
          @new-elements-a))
     )
    (when (= (count
               @new-elements)
             (count
               @indexes-to-replace))
      (dotimes [i (count
                  data-vector)]
        (let [index-to-replace (atom
                                 (first
                                   @indexes-to-replace))]
          ((fn []
             (when (and (not
                          (nil?
                            @index-to-replace))
                        (< @index-to-replace
                           0))
               (swap!
                 new-elements
                 remove-first-fn)
               (swap!
                 indexes-to-replace
                 remove-first-fn)
               (reset!
                 index-to-replace
                 (first
                   @indexes-to-replace))
               (recur))
            ))
          (when (= @index-to-replace
                   i)
            (let [new-element (first
                                @new-elements)]
              (swap!
                result
                conj
                new-element)
              (swap!
                new-elements
                remove-first-fn)
              (swap!
                indexes-to-replace
                remove-first-fn))
           )
          (when (not= @index-to-replace
                      i)
            (swap!
              result
              conj
              (get
                data-vector
                i))
           ))
        ))
    @result))

(defn insert-in-vector-on-index
  "Insert data in vector
  
   data-vector is vector with elements of any data type
   element is single element or vector of elements that are going to be inserted
    in data vector
   index is single number or vector of numbers that represent positions in data-vector
    where element/s will be inserted"
  [data-vector
   element
   index]
  (let [[new-elements
         indexes-to-replace] (if (number?
                                   index)
                               [(atom
                                  [element])
                                (atom
                                  [index])]
                               (when (and (vector?
                                            element)
                                          (vector?
                                            index))
                                 [(atom
                                    element)
                                  (atom
                                    index)])
                              )
        remove-first-fn (fn [param]
                          (into
                            []
                            (rest
                              param))
                         )
        result (atom [])]
    (when (< (count
               @new-elements)
             (count
               @indexes-to-replace))
      (let [new-indexes (atom [])]
        (dotimes [i (count
                      @new-elements)]
          (swap!
            new-indexes
            conj
            (get
              @indexes-to-replace
              i))
         )
        (reset!
          indexes-to-replace
          @new-indexes))
     )
    (when (< (count
               @indexes-to-replace)
             (count
               @new-elements))
      (let [new-elements-a (atom [])]
        (dotimes [i (count
                      @indexes-to-replace)]
          (swap!
            new-elements-a
            conj
            (get
              @new-elements
              i))
         )
        (reset!
          new-elements
          @new-elements-a))
     )
    (when (= (count
               @new-elements)
             (count
               @indexes-to-replace))
      (dotimes [i (count
                  data-vector)]
        (let [index-to-replace (atom
                                 (first
                                   @indexes-to-replace))]
          ((fn []
             (when (and (not
                          (nil?
                            @index-to-replace))
                        (< @index-to-replace
                           0))
               (swap!
                 new-elements
                 remove-first-fn)
               (swap!
                 indexes-to-replace
                 remove-first-fn)
               (reset!
                 index-to-replace
                 (first
                   @indexes-to-replace))
               (recur))
            ))
          (when (= @index-to-replace
                   i)
            (let [new-element (first
                                @new-elements)]
              (swap!
                result
                conj
                new-element)
              (swap!
                new-elements
                remove-first-fn)
              (swap!
                indexes-to-replace
                remove-first-fn))
           )
          (swap!
            result
            conj
            (get
              data-vector
              i))
         ))
     )
    @result))

(defn read-file
  "example:
  file-pathe  public/index.html"
  [file-path]
 ; (clojure.core/slurp	(resource	file-path))
  )

(def pass-key
     "password-key")

(defn encrypt-password
  "Encrypt password"
  [password]
  #?(:clj
      (let [pass-key-len (count
                           pass-key)
            pass-len (count
                       password)
            encrypted-password (atom "")]
        (doseq [itr (range
                      pass-len)]
          (let [key-char (get
                           password
                           itr)
                pass-key-char (get
                                pass-key
                                (mod
                                  itr
                                  pass-key-len))
                key-char-code (int
                                key-char)
                pass-key-char-code (int
                                     pass-key-char)
                key-pass-code (+ key-char-code
                                 pass-key-char-code)
                key-pass-char (char
                                key-pass-code)]
            (swap!
              encrypted-password
              str
              key-pass-char))
         )
         @encrypted-password)
     :cljs
      (let [pass-key-len (count
                           pass-key)
            pass-len (count
                       password)
            encrypted-password (atom "")]
        (doseq [itr (range
                      pass-len)]
          (let [key-char (aget
                           password
                           itr)
                pass-key-char (aget
                                pass-key
                                (mod
                                  itr
                                  pass-key-len))
                key-char-code (.charCodeAt
                                key-char
                                0)
                key-pass-code (+ (.charCodeAt
                                   key-char
                                   0)
                                 (.charCodeAt
                                   pass-key-char
                                   0))
                key-pass-char (.fromCharCode
                                js/String
                                key-pass-code)]
            (swap!
              encrypted-password
              str
              key-pass-char))
         )
         @encrypted-password))
  )

(defn decrypt-password
  "Decrypt password"
  [encrypted-password]
  #?(:clj
      (let [pass-key-len (count
                           pass-key)
            pass-len (count
                       encrypted-password)
            decrypted-password (atom "")]
        (doseq [itr (range
                      pass-len)]
          (let [key-char (get
                           encrypted-password
                           itr)
                pass-key-char (get
                                pass-key
                                (mod
                                  itr
                                  pass-key-len))
                key-char-code (int
                                key-char)
                pass-key-char-code (int
                                     pass-key-char)
                key-pass-code (- key-char-code
                                 pass-key-char-code)
                key-pass-char (char
                                key-pass-code)]
            (swap!
              decrypted-password
              str
              key-pass-char))
         )
         @decrypted-password)
     :cljs
      (let [pass-key-len (count
                           pass-key)
            pass-len (count
                       encrypted-password)
            decrypted-password (atom "")]
        (doseq [itr (range pass-len)]
          (let [key-char (aget
                           encrypted-password
                           itr)
                pass-key-char (aget
                                pass-key
                                (mod
                                  itr
                                  pass-key-len))
                key-char-code (.charCodeAt
                                key-char
                                0)
                key-pass-code (- (.charCodeAt
                                   key-char
                                   0)
                                 (.charCodeAt
                                   pass-key-char
                                   0))
                key-pass-char (.fromCharCode
                                js/String
                                key-pass-code)]
            (swap!
              decrypted-password
              str
              key-pass-char))
         )
         @decrypted-password))
  )

(def js-sha256
     "function sha256() {function rightRotate(value, amount) {return (value>>>amount) | (value<<(32 - amount));};var ascii = \"CHANGE_THIS_STRING\";var mathPow = Math.pow;var maxWord = mathPow(2, 32);var lengthProperty = 'length';var i, j;var result = '';var words = [];var asciiBitLength = ascii[lengthProperty]*8;var hash = sha256.h = sha256.h || [];var k = sha256.k = sha256.k || [];var primeCounter = k[lengthProperty];var isComposite = {}; for (var candidate = 2; primeCounter < 64; candidate++) {if (!isComposite[candidate]) { for (i = 0; i < 313; i += candidate) {isComposite[i] = candidate;}hash[primeCounter] = (mathPow(candidate, .5)*maxWord)|0;k[primeCounter++] =(mathPow(candidate, 1/3)*maxWord)|0;}}ascii += '\\x80';while (ascii[lengthProperty]%64 - 56) ascii += '\\x00'; for (i = 0; i < ascii[lengthProperty]; i++) {j = ascii.charCodeAt(i);if (j>>8) return;words[i>>2] |= j << ((3 - i)%4)*8;}words[words[lengthProperty]] = ((asciiBitLength/maxWord)|0);words[words[lengthProperty]] = (asciiBitLength); for (j = 0; j < words[lengthProperty];) {var w = words.slice(j, j += 16);var oldHash = hash;hash = hash.slice(0, 8); for (i = 0; i < 64; i++) {var i2 = i + j;var w15 = w[i - 15], w2 = w[i - 2];var a = hash[0], e = hash[4];var temp1 = hash[7] + (rightRotate(e, 6) ^ rightRotate(e, 11) ^ rightRotate(e, 25)) + ((e&hash[5])^((~e)&hash[6])) + k[i] + (w[i] = (i < 16) ? w[i] : (w[i - 16] + (rightRotate(w15, 7) ^ rightRotate(w15, 18) ^ (w15>>>3)) + w[i - 7] + (rightRotate(w2, 17) ^ rightRotate(w2, 19) ^ (w2>>>10)))|0);var temp2 = (rightRotate(a, 2) ^ rightRotate(a, 13) ^ rightRotate(a, 22)) + ((a&hash[1])^(a&hash[2])^(hash[1]&hash[2])); hash = [(temp1 + temp2)|0].concat(hash);hash[4] = (hash[4] + temp1)|0;} for (i = 0; i < 8; i++) { hash[i] = (hash[i] + oldHash[i])|0;}} for (i = 0; i < 8; i++) { for (j = 3; j + 1; j--) {var b = (hash[i]>>(j*8))&255; result += ((b < 16) ? 0 : '') + b.toString(16);}} return result;} sha256();")

(defn sha256
  "Simple text to sha 256"
  [ascii]
  #?(:clj
      (let [digest (java.security.MessageDigest/getInstance
                     "SHA-256")
            hash-byte-array (.digest
                              digest
                              (.getBytes
                                ascii
                                java.nio.charset.StandardCharsets/UTF_8))
            hex-string (atom "")]
        (doseq [hash-byte hash-byte-array]
          (let [hex (Integer/toHexString
                      (bit-and 0xff
                           hash-byte))
                hex-length (count
                             hex)]
            (when (= hex-length
                     1)
              (swap!
                hex-string
                str
                "0"))
            (swap!
              hex-string
              str
              hex))
         )
        @hex-string)
     :cljs
      (let [replaced-js-sha256 (cstring/replace-first
                                 js-sha256
                                 "CHANGE_THIS_STRING"
                                 ascii)
            hex-string (js/eval
                         replaced-js-sha256)]
        hex-string))
 )

(defn current-date
  "Returns formatted current date"
  []
  #?(:clj
      (let [sdf (java.text.SimpleDateFormat.
                  "E, dd MMM yyyy, HH:mm:ss zzz")
            date (java.util.Date.)]
        (.setTimeZone
          sdf
          (java.util.TimeZone/getTimeZone
            "GMT+1"))
        (.format
          sdf
          date))
     :cljs
      (let [date (js/Date.)]
        (.toLocaleString
          date
          "en-GB"
          (js-obj
            "timeZone" "Europe/Belgrade"
            ;"timeZoneName" "short"
            "weekday" "short"
            "year" "numeric"
            "month" "short"
            "day" "numeric"
            "hour" "2-digit"
            "minute" "2-digit"
            "second" "2-digit"
            "hour12" false))
       ))
 )

(defn split-with-newline
  "Split text with newline without loosing empty rows"
  [text]
  (let [all-rows (atom [])
        current-row (atom "")]
    (doseq [c text]
      (if (= c
             \newline)
        (do
          (swap!
            all-rows
            conj
            (swap!
              current-row
              str
              \newline))
          (reset!
            current-row
            ""))
         (swap!
           current-row
           str
           c))
     )
    (when-not (empty?
                @current-row)
      (swap!
        all-rows
        conj
        @current-row))
    @all-rows))

(defn is-number?
  "Check if parameter is number or NaN"
  [param]
  #?(:clj (number?
            param)
     :cljs
      (and (number?
             param)
           (not
             (.isNaN
               js/Number
               param))
       ))
 )

(defn nth-root
  "Calculates nth root of passed number"
  [base
   n]
  #?(:clj (when (and base
                     (number?
                       base)
                     n
                     (number?
                       n))
            (Math/pow
              Math/E
              (/ (Math/log
                   base)
                 n))
           )
     :cljs (when (and base
                     (number?
                       base)
                     n
                     (number?
                       n))
             (.pow
               js/Math
               (aget
                 js/Math
                 "E")
               (/ (.log
                    js/Math
                    base)
                  n))
            ))
 )

(defn calculate-circle-coordinates
  "Calculates circle coordinates using diameter in pixels and angle in degrees"
  [r
   angle]
  #?(:clj (when (and r
                     (number?
                       r)
                     angle
                     (number?
                       angle))
            (let [t (/ (* Math/PI
                          angle)
                       180)
                  x (long
                      (* (Math/cos
                           t)
                         r))
                  y (long
                      (* (Math/sin
                           t)
                         r))]
              [x y]))
     :cljs (when (and r
                      (number?
                        r)
                      angle
                      (number?
                        angle))
             (let [t (/ (* (aget
                             js/Math
                             "PI")
                           angle)
                        180)
                   x (long
                       (* (.cos
                            js/Math
                            t)
                          r))
                   y (long
                       (* (.sin
                            js/Math
                            t)
                          r))]
               [x y]))
     ))

(defn get-quadrate-extreme-points
  "Returns quadrate extreme points"
  [quadrate-number
   radius]
  (when (and quadrate-number
             (number?
               quadrate-number)
             radius
             (number?
               radius))
    (let [result (atom nil)]
      (when (= quadrate-number
               0)
        (reset!
          result
          {:start [radius 0]
           :angle [radius radius]
           :end [0 radius]
           :start-angle 0
           :end-angle 90}))
      (when (= quadrate-number
               1)
        (reset!
          result
          {:start [0 radius]
           :angle [(- radius) radius]
           :end [(- radius) 0]
           :start-angle 90
           :end-angle 180}))
      (when (= quadrate-number
               2)
        (reset!
          result
          {:start [(- radius) 0]
           :angle [(- radius) (- radius)]
           :end [0 (- radius)]
           :start-angle 180
           :end-angle 270})
       )
      (when (= quadrate-number
               3)
        (reset!
          result
          {:start [0 (- radius)]
           :angle [radius (- radius)]
           :end [radius 0]
           :start-angle 270
           :end-angle 360}))
      @result))
 )

(defn find-quadrate
  "Finds quadrate in which coordinates are"
  [x
   y]
  (when (and x
             (number?
               x)
             y
             (number?
               y))
    (let [result (atom nil)]
      (when (and (<= 0
                     x)
                 (<= 0
                     y))
        (reset!
          result
          0))
      (when (and (< x
                    0)
                 (<= 0
                     y))
        (reset!
          result
          1))
      (when (and (< x
                    0)
                 (< y
                    0))
        (reset!
          result
          2))
      (when (and (<= 0
                     x)
                 (< y
                    0))
        (reset!
          result
          3))
      @result))
 )

(defn find-quadrate-by-angle
  "Finds quadrates that contain passed angles"
  [start-angle
   end-angle]
  (when (and start-angle
             (number?
               start-angle)
             end-angle
             (number?
               end-angle))
    (let [start-quadrate (atom 0)
          end-quadrate (atom 0)]
      (when (and (<= 0
                     start-angle)
                 (< start-angle
                    90))
        (reset!
          start-quadrate
          0))
      (when (and (< 0
                    end-angle)
                 (<= end-angle
                     90))
        (reset!
          end-quadrate
          0))
      (when (and (<= 90
                     start-angle)
                 (< start-angle
                    180))
        (reset!
          start-quadrate
          1))
      (when (and (< 90
                    end-angle)
                 (<= end-angle
                     180))
        (reset!
          end-quadrate
          1))
      (when (and (<= 180
                     start-angle)
                 (< start-angle
                    270))
        (reset!
          start-quadrate
          2))
      (when (and (< 180
                    end-angle)
                 (<= end-angle
                     270))
        (reset!
          end-quadrate
          2))
      (when (and (<= 270
                     start-angle)
                 (< start-angle
                    360))
        (reset!
          start-quadrate
          3))
      (when (and (< 270
                    end-angle)
                 (<= end-angle
                     360))
        (reset!
          end-quadrate
          3))
      (into
        []
        (range
          @start-quadrate
          (inc
            @end-quadrate))
       ))
   ))

(defn calculate-angle
  "Calculates PI representation of angle"
  [angle-degrees]
  (when (and angle-degrees
             (number?
               angle-degrees))
    (let [adapted-angle (mod
                          angle-degrees
                          360)
          adapted-angle (- 360
                           adapted-angle)]
      (/ (* #?(:clj Math/PI
               :cljs (aget
                       js/Math
                       "PI"))
            adapted-angle)
         180))
   ))

(defn calculate-angle-by-coordinates-asin
  "Calculates angle by coordinates asin"
  [x
   y]
  (when (and x
             (number?
               x)
             y
             (number?
               y)
             (not= y
                   0))
    #?(:clj (let [angle (Math/asin
                          (Math/sqrt
                            (/ 1
                               (+ 1
                                  (Math/pow
                                    (/ x
                                       y)
                                    2))
                             ))
                         )]
              (/ (* angle
                    180)
                 Math/PI))
       :cljs (let [angle (.asin
                           js/Math
                           (.sqrt
                             js/Math
                             (/ 1
                                (+ 1
                                   (.pow
                                     js/Math
                                     (/ x
                                        y)
                                     2))
                              ))
                          )]
               (/ (* angle
                     180)
                  (aget
                    js/Math
                    "PI"))
              ))
   ))

(defn calculate-angle-by-coordinates-acos
  "Calculates angle by coordinates acos"
  [x
   y]
  (when (and x
             (number?
               x)
             (not= x
                   0)
             y
             (number?
               y))
    #?(:clj (let [angle (Math/acos
                          (Math/sqrt
                            (/ 1
                               (+ 1
                                  (Math/pow
                                    (/ y
                                       x)
                                    2))
                             ))
                         )]
              (/ (* angle
                    180)
                 Math/PI))
       :cljs (let [angle (.acos
                           js/Math
                           (.sqrt
                             js/Math
                             (/ 1
                                (+ 1
                                   (.pow
                                     js/Math
                                     (/ y
                                        x)
                                     2))
                              ))
                          )]
               (/ (* angle
                     180)
                  (aget
                    js/Math
                    "PI"))
              ))
   ))

(defn calculate-radius-by-x
  "Calculate radius by x coordinate"
  [x
   angle]
  (when (and x
             (number?
               x)
             angle
             (number?
               angle))
    #?(:clj (let [angle-pi (/ (* Math/PI
                                 angle)
                              180)
                  cos-angle-pi (Math/cos
                                 angle-pi)]
              (when (not= cos-angle-pi
                          0.0)
                (/ x
                   cos-angle-pi))
             )
       :cljs (let [angle-pi (/ (* (aget
                                    js/Math
                                    "PI")
                                  angle)
                               180)
                   cos-angle-pi (.cos
                                  js/Math
                                  angle-pi)]
               (when (not= cos-angle-pi
                           0.0)
                 (/ x
                    cos-angle-pi))
              ))
   ))

(defn calculate-radius-by-y
  "Calculate radius by x coordinate"
  [y
   angle]
  (when (and y
             (number?
               y)
             angle
             (number?
               angle))
    #?(:clj (let [angle-pi (/ (* Math/PI
                                 angle)
                              180)
                  sin-angle-pi (Math/sin
                                 angle-pi)]
              (when (not= sin-angle-pi
                          0.0)
                (/ y
                   sin-angle-pi))
             )
       :cljs (let [angle-pi (/ (* (aget
                                    js/Math
                                    "PI")
                                  angle)
                               180)
                   sin-angle-pi (.sin
                                  js/Math
                                  angle-pi)]
               (when (not= sin-angle-pi
                           0.0)
                 (/ y
                    sin-angle-pi))
              ))
   ))

(defn rotate-x
  "Rotate x coordinate for radius and angle"
  [r
   angle]
  (when (and r
             (number?
               r)
             angle
             (number?
               angle))
    #?(:clj (let [angle-pi (/ (* Math/PI
                                 angle)
                              180)]
              (* r
                 (Math/cos
                   angle-pi))
             )
       :cljs (let [angle-pi (/ (* (aget
                                    js/Math
                                    "PI")
                                  angle)
                               180)]
               (* r
                  (.cos
                    js/Math
                    angle-pi))
              ))
   ))

(defn rotate-y
  "Rotate y coordinate for radius and angle"
  [r
   angle]
  (when (and r
             (number?
               r)
             angle
             (number?
               angle))
    #?(:clj (let [angle-pi (/ (* Math/PI
                                 angle)
                              180)]
              (* r
                 (Math/sin
                   angle-pi))
             )
       :cljs (let [angle-pi (/ (* (aget
                                    js/Math
                                    "PI")
                                  angle)
                               180)]
               (* r
                  (.sin
                    js/Math
                    angle-pi))
              ))
   ))

(defn rotate-coordinates
  "Rotates x and y coordinates"
  [x
   y
   angle]
  (when (and x
             (number?
               x)
             y
             (number?
               y)
             angle
             (number?
               angle))
    (let [c-angle (calculate-angle-by-coordinates-asin
                    x
                    y)
          c-angle (if c-angle
                    c-angle
                    (calculate-angle-by-coordinates-acos
                      x
                      y))
          radius (calculate-radius-by-x
                   x
                   c-angle)
          rotated-x (rotate-x
                      radius
                      (+ c-angle
                         angle))
          rotated-y (rotate-y
                      radius
                      (+ c-angle
                         angle))]
      [rotated-x
       rotated-y]))
 )

(defn decode-ascii
  "Decodes from utf-8 to ascii"
  [text]
  (if (string?
        text)
    (let [special-characters const/utf-8-ascii-mapping
          text-decoded-a (atom text)]
      (doseq [[escaped-special-character
               special-character] special-characters]
        (reset!
          text-decoded-a
          (cstring/replace
            @text-decoded-a
            special-character
            escaped-special-character))
       )
      @text-decoded-a)
    ""))

(defn format-bytes-number
  "Formats bytes in B, KB, MB, GB and TB"
  [number-of-bytes
   unit-type]
  (when (number?
          number-of-bytes)
    (case unit-type
      "B" (str
            (round-decimals
              number-of-bytes
              2)
            " B")
      "KB" (str
             (round-decimals
               (double
                 (/ number-of-bytes
                    1024))
               2)
             " KB")
      "MB" (str
             (round-decimals
               (double
                 (/ number-of-bytes
                    (* 1024
                       1024))
                )
               2)
             " MB")
      "GB" (str
             (round-decimals
               (double
                 (/ number-of-bytes
                    (* 1024
                       1024
                       1024))
                )
               2)
             " GB")
      "TB" (str
             (round-decimals
               (double
                 (/ number-of-bytes
                    (* 1024
                       1024
                       1024
                       1024))
                )
               2)
             " TB")
      (str
        (round-decimals
          number-of-bytes
          2)
        " B"))
   ))

(defn format-bytes-number-with-lowest-number-of-digits
  "Formats number of bytes with lowest number of digits"
  [number-of-bytes]
  (when (number?
          number-of-bytes)
    (let [result (atom number-of-bytes)]
      (when (< 0
               number-of-bytes
               1024)
        (reset!
          result
          (format-bytes-number
            number-of-bytes
            "B"))
       )
      (when (<= 1024
                number-of-bytes
                (* 1024
                   1024))
        (reset!
          result
          (format-bytes-number
            number-of-bytes
            "KB"))
       )
      (when (< (* 1024
                  1024)
                number-of-bytes
                (* 1024
                   1024
                   1024))
        (reset!
          result
          (format-bytes-number
            number-of-bytes
            "MB"))
       )
      (when (<= (* 1024
                   1024
                   1024)
                 number-of-bytes
                 (* 1024
                    1024
                    1024
                    1024))
        (reset!
          result
          (format-bytes-number
            number-of-bytes
            "GB"))
       )
      (when (< (* 1024
                  1024
                  1024
                  1024)
                number-of-bytes
                (* 1024
                   1024
                   1024
                   1024
                   1024))
        (reset!
          result
          (format-bytes-number
            number-of-bytes
            "TB"))
       )
      @result))
 )

