(ns utils-lib.core-clj
  (:require [clojure.java.shell :refer [sh]]))

(defn java-heap-size
  "Print out java heap memory usage"
  []
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
    {:used-memory (double used-memory)
     :free-memory (double free-memory)
     :total-memory (double total-memory)
     :max-memory (double max-memory)}))

(defn sh-exists?
  "Check if sh file exists"
  []
  (let [out (:out
              (sh
                "ls" "/tmp/sh"))]
    (not
      (empty?
        out))
   ))

(defn make-sh-file
  "Make sh file so it can be used in executing shell commands"
  []
  (when-not (sh-exists?)
    (try
      (sh "touch" "/tmp/sh")
      (sh "chmod" "755" "/tmp/sh")
      (let [file-path "/tmp/sh"
            file-content (str
                           "#!/bin/bash\n"
                           "for i do\n"
                           "  eval \"$i\"\n"
                           "done")
            f (java.io.File.
                file-path)
            ary (.getBytes
                  file-content
                  "UTF-8")
            os (java.io.FileOutputStream.
                 f)]
        (.write
          os
          ary)
        (.close
          os))
      (catch Exception e
        (println (.getMessage e))
       ))
   ))

(defn execute-shell-command
  "Execute shell command from sh file at it's file path"
  [command]
  (make-sh-file)
  (let [final-command (atom 
                        ["/tmp/sh"])
        result (atom nil)]
    (when (string?
            command)
      (swap!
        final-command
        conj
        command)
      (reset!
        result
        (apply
          sh
          @final-command))
     )
    (when (vector?
            command)
      (doseq [cmd command]
        (swap!
          final-command
          conj
          cmd))
      (reset!
        result
        (apply
          sh
          @final-command))
     )
    @result))

