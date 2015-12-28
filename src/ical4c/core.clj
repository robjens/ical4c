(ns ical4c.core
  "One thing to keep in mind, is that iCal Calendar vEvents are added
  through the use of java.util.Calendar"
  (:refer-clojure :exclude [second read-string])
  (:require (clj-time [core :as t]
                      [coerce :as c])
            (clojure [string :as string]
                     [edn :refer (read-string)])
            (schema [core :as s]
                    [coerce :as sc])
            (schema.spec [core :as spec])
            (schema-tools [core :as st]
                          [coerce :as stc])
            [me.raynes.fs :as fs]
            [ical4c.macros :refer (import-static)]
            (clojure [string :refer (lower-case)]
                     [pprint :refer (pprint)])
            [plumbing.core :refer (defnk fnk map-keys ?>)]
            (clojure.core [reducers :as r])
            [backtick :refer (template syntax-quote)]
            [clojure.java.io :as io :refer (input-stream)])
  (:import (net.fortuna.ical4j.data CalendarBuilder)
           (net.fortuna.ical4j.model.property Version ProdId CalScale)
           (net.fortuna.ical4j.model.parameter Value)
           (net.fortuna.ical4j.model Date Dur Calendar Property
                                     ComponentList PropertyList ParameterList
                                     TimeZoneRegistry TimeZoneRegistryFactory TimeZone)
           (net.fortuna.ical4j.model.component VTimeZone VEvent)
           (java.util.Calendar)
           (java.util GregorianCalendar)
           (com.google.common.base CaseFormat)))

(s/defschema
  )

;;;
;;; Static Const Values (bitmasks)
;;;

;; See macros.clj: expose lower-hyphen symbol names in scope
(import-static com.google.common.base.CaseFormat
               LOWER_CAMEL LOWER_HYPHEN LOWER_UNDERSCORE
               UPPER_CAMEL UPPER_UNDERSCORE)

;; Although we have clj-time / jodatime we'll still want these puppies since the
;; ical4j library class constructors eat them for (bitflag) properties to set.
;; clj-time makes us do (t/day-of-month (c/to-date (t/now)))
(import-static java.util.Calendar
               DATE ERA YEAR MONTH HOUR_OF_DAY MINUTE SECOND MILLISECOND
               JANUARY FEBRUARY MARCH APRIL MAY JUNE JULY AUGUST SEPTEMBER OCTOBER NOVEMBER DECEMBER
               MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY
               WEEK_OF_MONTH WEEK_OF_YEAR
               DAY_OF_MONTH DAY_OF_WEEK DAY_OF_WEEK_IN_MONTH DAY_OF_YEAR
               HOUR ZONE_OFFSET)


(import-static net.fortuna.ical4j.model.Property
               VERSION SUMMARY DTSTAMP DTSTART DTEND)

(import-static net.fortuna.ical4j.model.parameter.Value
               DATE)


(defn integerify
  "Tries to convert a string to an integer or returns default-value/0 otherwise.
  This is useful in a few ways, e.g. when using db values for settings to
  fallback to as sanity/safety measure."
  ([a-string default-value]
   (try
     (if (or (nil? a-string)
             (empty? a-string))
       default-value
       (Integer/valueOf a-string))
     (catch Exception e default-value)))
  ([maybe-string] (if (number? maybe-string)
                    maybe-string
                    (integerify maybe-string maybe-string))))


;;;
;;; Schema Coercions
;;;
(defn base-matcher [schema-pred value-pred value-fn]
  (fn [schema]
    (if (schema-pred schema)
      (fn [x]
        (if (value-pred x)
          (value-fn x))))))


(def var-gets #(if (var? (resolve %))
                 (var-get (resolve %))
                 (keyword (str %)))) ;?? shall we do this? only need symbols that exist me thinks

(def bitmask (comp var-gets symbol name))
(defn string-or-number? [x] (or (string? x) (number? x)))
(def m-keyword|int (base-matcher #(= (s/either s/Int s/Keyword) %) keyword? bitmask))
;; (def m2 (base-matcher #(= s/Num %) number? identity))

;; matches on schema of either number or string, takes string input and returns integer if parsable or else string
(def m-str|num-or-str (base-matcher #(= (s/either s/Num s/Str) %) string-or-number? integerify))

(def coerce-bitmask
  (sc/coercer {(s/either s/Int s/Keyword) (s/either s/Num s/Str)}
              (stc/or-matcher m-keyword|int m-str|num-or-str)))
;; year is a special keyword -> bitflag integer 1 (see also :month, :day-of-month, :hour-of-day etc)
;; (coerce-bitmask {:year "2014"}) ;=> {1 2014}
;; (coerce-bitmask {:year "a2014"}) ;=> {1 "a2014"}
;; (coerce-bitmask {:foo "215"}) ;=> {:foo 215}
;; (coerce-bitmask {:foo "nbar"}) ;=> {:foo "nbar"}
;; (coerce-bitmask {:foo 1}) ;=> {:foo 1}
;; (coerce-bitmask {:day-of-month 20}) ;=> {5 20}



;;;
;;; TimeZone management
;;;

(s/defn ^{:private true :always-validate true :no-doc true}
  new-tz-registry :- TimeZoneRegistry
  [] (-> (TimeZoneRegistryFactory/getInstance)
         .createRegistry))
;; (new-tz-registry)
;; (meta (var new-tz-registry))

(defnk tz :- TimeZone
  [{tzone :- s/Str "Europe/Amsterdam"}
   {tzreg :- TimeZoneRegistry (new-tz-registry)}]
  (-> tzreg (.getTimeZone tzone)))
;; (tz {}) ;=> #object



;;;
;;; Java native Calendar
;;;

(s/defn new-jcal :- GregorianCalendar
  "Create a new Java Calendar object."
  [props & tzone]
  (let [tzone (if-not (empty? tzone) (first tzone) (tz {}))
        inst (java.util.Calendar/getInstance tzone)]
    (doseq [[k v] (-> props ksi seq)]
      (.set inst k v))
    inst))


;; (new-jcal {:year 2016 :month :january :day-of-month 1 ;; date
;;            :hour-of-day 13 :minute 30 :second 0 ;; time
;;            })


;;;
;;; iCal
;;;

(defn get-properties
  "Returns all properties from a calendar or component as a sorted-map
  of keywords and values."
  [x] (->> x .getProperties seq (r/map #(hash-map (.getName %)
                                                  (.getValue %)))
           (into (sorted-map))
           (map-keys (comp keyword lower-case))))

(s/defn get-components
  "Returns all components from a given calendar as sorted-map keyword
  keys and their properties nested in the same fashion."
  [cal :- Calendar]
  (->> cal .getComponents seq (r/map #(hash-map (.getName %)
                                                (get-properties %)))
       (into (sorted-map))
       (map-keys (comp keyword lower-case))))

(s/defn get-parameters
  "Get the parameters from a iCal property."
  [prop :- Property] (-> prop .getParameters))


;;;
;;; Duration
;;;

(s/defn new-duration :- Dur
  ([start :- Date, end :- Date] (Dur. start end))
  ([days :- s/Num, hrs :- s/Num, mins :- s/Num, secs :- s/Num]
   (Dur. days hrs mins secs))
  )

(s/defn negate-duration :- Dur
  [dur :- Dur] (.negate dur))

;; (negate-duration )

;; (new-duration
;;  (c/to-date (t/now))
;;  (c/to-date (t/plus (t/now) (t/hours 2))))

;;;
;;; vEvent
;;;

(s/defn new-event :- VEvent
  ; GregorianCalendar[java.util.Date] -> net.fortuna.ical4j.model.Date
  ([start, desc :- s/Str] (VEvent. start desc))
  ([start, end-or-dur, desc :- s/Str]
   (VEvent. (-> start c/to-date Date.)
            end-or-dur desc)))




(let [sd (-> (t/now) (t/plus (t/hours 200)) c/to-date Date.)
      e (new-jcal {:year 2016 :month :january :day-of-month 1
                   :hour-of-day 14 :minute 0 :second 0 :millisecond 0})
      ed (-> e .getTime Date.)]
  (new-event sd ed
             "New Year dive Scheveningen ^^")
  sd
)




;; ;; predicates
;; (defn ics? [s] (= ".ics" (fs/extension s)))
;; (defn prodid? [x] (= (type x) ProdId))
;; (defn properties? [xs] (= (type xs) PropertyList))
;; (defn components? [xs] (= (type xs) ComponentList))

;; ;; schemadef
;; (def IcsFile (s/both (s/pred fs/file? 'fs/file?)
;;                      (s/pred ics? 'ics?)))
;; ;; (s/validate IcsFile "/home/susy/Downloads/PayDay.ics")


;; (s/defn expand :- {:properties (s/maybe [s/Any])
;;                    :components (s/maybe [s/Any])}
;;   "Takes a calendar and maybe returns properties and/or components in a clj kvs"
;;   [cal :- Calendar]
;;   {:properties (-> cal get-properties)
;;    :components (-> cal get-components)})

;; (s/defn read-calendar :- Calendar
;;   "Reads a .ics file from disk given a file path."
;;   [file-path :- IcsFile]
;;   (.build (CalendarBuilder.) (input-stream file-path)))

;; ;; (-> "/home/susy/Downloads/PayDay.ics"
;; ;;     read-calendar
;; ;;      )


;; (defn add-property [x p] (-> x .getProperties (.add p)))
;; (defn get-property [x p] (-> x .getProperties (.getProperty p)))
;; (defn add-parameter [x v] (-> x .getParameters (.add v)))

;; (s/defn create-calendar-with-props :- Calendar
;;   "Takes a optional seq of properties and returns a calendar object with those
;;   properties attached/set to it and returns the calendar."
;;   [& properties]
;;   (let [cal (Calendar.)]
;;     ;; side effects
;;     (doseq [p properties] (add-property cal p))
;;     cal))
;; ;; (create-calendar-with-props (ProdId. "-//Foo//Bar 1.0//EN") Version/VERSION_2_0)

;; ;; named properties with sane defaults and string literal input allowed
;; (defnk create-calendar
;;   [{prodid :- (s/maybe (s/either s/Str ProdId)) nil}]
;;   (let [pid (when prodid
;;               (if (string? prodid) (ProdId. prodid)
;;                 (when (prodid? prodid) prodid)))
;;         xs (remove nil? [pid Version/VERSION_2_0 CalScale/GREGORIAN])]
;;     (apply create-calendar-with-props xs)))

;; ;; (create-calendar {})
;; ;; (create-calendar {:prodid (ProdId. "-//SevenMatches BV//SevenMatches 1.0//NL")})
;; ;; (create-calendar {:prodid "-//SevenMatches BV//SevenMatches 1.0//NL"})

;; ;(new-event (new-jcal {:props [month december day-of-month 25]}) "Christmas")

;; ;; (def xmas
;; ;;   (new-event (new-jcal {:props [year 2015
;; ;;                                 month december
;; ;;                                 day-of-month 25]})
;; ;;              "Christmas 2015"))

;; ;; (-> xmas
;; ;;     (get-property dtstart)
;; ;; ;;     (add-parameter date)
;; ;;     )

;; ;; xmas
;; ;; can't define schema just yet
;; ;; VTimeZone VEvent can be output at least but there are more

;; ;;      pprint
;; ;; ;;     get-components
;; ;; ;;      get-properties
;; ;; ;;      type
;; ;;     )






;; (def lh->uu #(-> lower-hyphen (.to upper-underscore %)))
;; (def keyword->UPPER_SYMBOL (comp symbol lh->uu name))
;; (defn symbolize-upper-from-keys [ks] (map keyword->UPPER_SYMBOL ks))
;;       ~(-> jns str string/lower-case (string/split #"[.]") last)
;; (import-static-by-keys
;;  'java.util.Calendar
;;  'OrdinalMonth [:january :february :march :april :may :june :july :august :september :october :november :december]
;;  'DtComponent [:year :month :day-of-month :hour :minute :second]
;;  'WeekDay [:monday :tuesday :wednesday :thursday :friday :saturday :sunday]
;;  'Other [:date :era :week-of-year :hour-of-day :day-of-week :day-of-week-in-month :day-of-year]
;;  )




;; ;; we're going to want to have some mapping from the bitmask flag constants to keywords
;; ;; this will allow easier interaction via e.g. API

;; ;; first a simple helper to build a lenient enum which takes only keywords
;; ;; and returns strings as well
;; (defn name-id-enum [xs]
;;   (apply s/enum (-> (juxt name identity) (map xs) flatten)))

;; ;; define a date time component schema
;; (def DateTimeComponentSchema
;;   (s/enum :year :month :day-of-month :hour :minute :second))

;; (def parse-dt-comp
;;   (coerce/coercer DateTimeComponentSchema coerce/json-coercion-matcher))
;; ;;(parse-dt-comp "year") ;=> :year

;; ;; (map-symbols-dt-comp :year) ;=> ::year
;; (s/defn ^:always-validate
;;   map-symbols-dt-comp :- s/Num
;;   [k :- DateTimeComponentSchema]
;;   (condp = k
;;     :year year :month month :day-of-month day-of-month
;;     :hour hour :minute minute :second second))

;; (def OrdinalMonthSchema
;;   (s/enum :january :february :march :april :may :june :july
;;           :august :september :october :november :december))

;; (def parse-ordinal-month
;;   (coerce/coercer OrdinalMonthSchema coerce/json-coercion-matcher))

;; (s/defn ^:always-validate
;;   map-symbols-ordinal-month
;;   [k :- OrdinalMonthSchema]
;;   (condp = k
;;     :january january :february february :march march :april april
;;     :may may :june june :july july :august august :september september
;;     :october october :november november :december december))

;; (def WeekDaysSchema
;;   (s/enum :monday :tuesday :wednesday :thursday
;;           :friday :saturday :sunday))

;; (def parse-week-day
;;   (coerce/coercer WeekDaysSchema coerce/json-coercion-matcher))

;; (s/defn ^:always-validate
;;   map-symbols-weekdays
;;   [k :- WeekDaysSchema]
;;   (condp = k
;;     :monday monday :tuesday tuesday :wednesday wednesday
;;     :thursday thursday :friday friday :saturday saturday
;;     :sunday sunday))


