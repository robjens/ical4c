(ns ical4c.core-test
  (:require [clojure.test :refer :all]
            [ical4c.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


;; year is a special keyword -> bitflag integer 1 (see also :month, :day-of-month, :hour-of-day etc)
;; (coerce-bitmask {:year "2014"}) ;=> {1 2014}
;; (coerce-bitmask {:year "a2014"}) ;=> {1 "a2014"}
;; (coerce-bitmask {:foo "215"}) ;=> {:foo 215}
;; (coerce-bitmask {:foo "nbar"}) ;=> {:foo "nbar"}
;; (coerce-bitmask {:foo 1}) ;=> {:foo 1}
;; (coerce-bitmask {:day-of-month 20}) ;=> {5 20}
;; (coerce-bitmask {:year 2016 :month "2012"}) ;=> {1 2016, 2 2012}
;; (coerce-bitmask {:year 2016 :month 2})
;; (coerce-bitmask {:year 2016 :month :january :day-of-month 1 ;; date
;;                  :hour-of-day 13 :minute 30 :second 0 ;; time
;;                  }) ; {1 2016, 2 0, 5 1, 11 13, 12 30, 13 0}



;; (new-jcal {:year 2016 :month :january :day-of-month 10 ;; date
;;            :hour-of-day 13 :minute 30 :second 0 ;; time
;;            })


;; (negate-duration )

;; (new-duration
;;  (c/to-date (t/now))
;;  (c/to-date (t/plus (t/now) (t/hours 2))))


;; demonstrate 2 methods of parsing dates/times
;; (let [; use clj-time to generate some computed date coerced to java base
;;       sd (-> (t/now) (t/plus (t/hours 122)) c/to-date Date.)
;;       ; use custom native java sugar keyword to symbol resolution and coercion
;;       e (new-jcal {:year 2016 :month :january :day-of-month 10
;;                    :hour-of-day 14 :minute 0 :second 0 :millisecond 0}) ; native java sugar
;;       ed (-> e .getTime Date.) ; then extract the date
;;       ]
;;   (->> "New Years Dive" (new-event sd ed)
;;        str println)
;; )


;; (-> (new-duration 2 12 30 0)
;;     str) ;=> "P2DT12H30M"


;; (-> "/home/susy/Downloads/PayDay.ics"
;;     read-calendar
;;      )

;; (prodid {}) ; "-//ical4j//ical4c 1.0//NL"



;; (-> (read-calendar "/home/susy/Downloads/PayDay.ics")
;;     (write-calendar "/tmp/doto.ics"))



;; (create-attendee [["jan@jansen.nl" req-participant "Dev 1"]
;;                   ["dolf@jansen.nl" req-participant "DJ"]
;;                   ])
