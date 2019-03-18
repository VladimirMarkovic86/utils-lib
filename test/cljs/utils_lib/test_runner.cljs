(ns utils-lib.test-runner
  (:require [utils-lib.core-test]
            [utils-lib.core-test-cljs]
            [doo.runner :refer-macros [doo-tests doo-all-tests]]))

(enable-console-print!)

(doo-tests
  'utils-lib.core-test
  'utils-lib.core-test-cljs)

