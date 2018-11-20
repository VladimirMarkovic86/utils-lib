(ns utils-lib.core)

(defn round-up
  "Round up divided numbers"
  [number1
   number2]
  (if (= 0 (mod number1
                number2))
   (int (/ number1
           number2))
   (inc 
    (int (/ number1
            number2))
    ))
  )

(defn round-decimals
  "Round number to particular number of decimals"
  [number
   decimals-num]
  (let [number-i (int number)
        number-ii (atom (- number
                           number-i))
        divider (atom 1)]
   (doseq [itr (range decimals-num)]
    (swap! number-ii * 10))
   (swap! number-ii int)
   (doseq [itr (range decimals-num)]
    (swap! divider * 10))
   (swap! number-ii / @divider)
   (swap! number-ii double)
   (+ number-i
      @number-ii))
  )

(defn find-index-to-remove
  "Additional function for remove-index-from-vector fn
   determine if index and data at that index in vector should be removed"
  [itr
   index-to-remove
   current-index]
  (if (< current-index (count index-to-remove))
   (if (= itr (index-to-remove current-index))
    current-index
    (recur itr
           index-to-remove
           (inc current-index))
    )
   false))

(defn remove-index-from-vector
  "Remove data at particular index or indexes
   
   data-vector is simple vector with elements of any data type
   index is single number od vector of numbers that represent index/es
   that should be removed"
  [data-vector
   index]
  (let [removed-index  (reduce (fn [acc
                                    elem]
                                (let [itr  (:itr acc)
                                      index-to-remove  (:index-to-remove acc)
                                      result  (:result acc)
                                      compared-index  (find-index-to-remove itr
                                                                            index-to-remove
                                                                            0)]
                                 (if compared-index
                                  (if (< (count index-to-remove) 2)
                                   {:itr (inc itr)
                                    :index-to-remove []
                                    :result result}
                                   {:itr (inc itr)
                                    :index-to-remove (remove-index-from-vector
                                                      index-to-remove
                                                      compared-index)
                                    :result result})
                                  {:itr (inc itr)
                                   :index-to-remove index-to-remove
                                   :result (conj result elem)})
                                 ))
                               {:itr 0
                                :index-to-remove (if (vector? index)
                                                  index
                                                  [index])
                                :result []}
                               data-vector)]
   (:result removed-index))
  )

(defn replace-in-vector-on-index
  "Replace data in vector on particular indexes
   
   data-vector is vector with elements of any data type
   element is single element or vector of elements that are going to replace old data
   index is single number or vector of numbers that represent positions in data vector
    where replacement/s will take place"
  [data-vector
   element
   index]
  (let [replaced-elements  (reduce (fn [acc
                                        elem]
                                    (let [itr  (:itr acc)
                                          replace-on-index  (:replace-on-index acc)
                                          replace-element  (:replace-element acc)
                                          result  (:result acc)
                                          compared-index  (find-index-to-remove
                                                           itr
                                                           replace-on-index
                                                           0)]
                                     (if compared-index
                                      {:itr  (inc itr)
                                       :replace-on-index  (remove-index-from-vector
                                                           replace-on-index
                                                           compared-index)
                                       :replace-element  (remove-index-from-vector
                                                          replace-element
                                                          compared-index)
                                       :result (conj result
                                                     (replace-element
                                                      compared-index))}
                                      {:itr (inc itr)
                                       :replace-on-index replace-on-index
                                       :replace-element replace-element
                                       :result (conj result
                                                     elem)})
                                     ))
                                   {:itr 0
                                    :replace-on-index (if (vector?
                                                            index)
                                                        index
                                                        [index])
                                    :replace-element (if (vector?
                                                           index)
                                                       element
                                                       [element])
                                    :result []}
                                   data-vector)]
   (:result replaced-elements))
  )

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
  (let [inserted-elements  (reduce
                             (fn [acc
                                  elem]
                              (let [itr (:itr acc)
                                    insert-on-index (:insert-on-index acc)
                                    insert-element (:insert-element acc)
                                    result  (:result acc)]
                               (if (= itr insert-on-index)
                                {:itr  (inc itr)
                                 :insert-on-index -1
                                 :insert-element []
                                 :result (reduce
                                           conj
                                           result
                                           insert-element)}
                                {:itr (inc itr)
                                 :insert-on-index insert-on-index
                                 :insert-element insert-element
                                 :result (conj
                                           result
                                           elem)}))
                              )
                             {:itr 0
                              :insert-on-index index
                              :insert-element (if (vector?
                                                    element)
                                                element
                                                [element])
                              :result []}
                             data-vector)]
   (:result inserted-elements))
  )

(defn read-file
  "example:
  file-pathe  public/index.html"
  [file-path]
 ; (clojure.core/slurp	(resource	file-path))
  )

(def pass-key "password-key")

(defn encrypt-password
  "Encrypt password"
  [password]
  #?(:clj
      (let [pass-key-len (count pass-key)
            pass-len (count password)
            encrypted-password (atom "")]
        (doseq [itr (range pass-len)]
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
      (let [pass-key-len (count pass-key)
            pass-len (count password)
            encrypted-password (atom "")]
        (doseq [itr (range pass-len)]
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
      (let [pass-key-len (count pass-key)
            pass-len (count
                       encrypted-password)
            decrypted-password (atom "")]
        (doseq [itr (range pass-len)]
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
      (let [pass-key-len (count pass-key)
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

(defn java-heap-size
  "Print out java heap memory usage"
  []
  #?(:clj
      (let [mb (* 1024
                  1024)
            runtime (Runtime/getRuntime)
            used-memory (/ (- (.totalMemory
                                runtime)
                              (.freeMemory
                                runtime))
                           mb)
            free-memory (/ (.freeMemory
                             runtime)
                           mb)
            total-memory (/ (.totalMemory
                               runtime)
                             mb)
            max-memory (/ (.maxMemory
                            runtime)
                          mb)]
        (println "##### Heap utilization statistics [MB] #####")
        (println "Used Memory: " (double used-memory))
        (println "Free Memory: " (double free-memory))
        (println "Total Memory:" (double total-memory))
        (println "Max Memory:" (double max-memory))
       ))
 )

(defn current-date
  "Returns formatted current date"
  []
  #?(:clj
      (let [sdf (java.text.SimpleDateFormat.
                  "E, dd MMM yyyy HH:mm:ss zzz")
            date (java.util.Date.)]
        (.setTimeZone
          sdf
          (java.util.TimeZone/getTimeZone
            "GMT"))
        (.format
          sdf
          date))
     ))

(defn parse-body
  "Read entity-body from request, convert from string to clojure data"
  [request]
  #?(:clj (try
            (read-string
              (:body request))
            (catch Exception e
              ;(println (.getMessage e))
             ))
     ))

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
    (swap!
      all-rows
      conj
      @current-row)
    @all-rows))

