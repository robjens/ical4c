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
            (schema-tools [core :as st]
                          [coerce :as stc])
            [me.raynes.fs :as fs]
            [ical4c.macros :refer (import-static)]
            (clojure [string :refer (lower-case)]
                     [pprint :refer (pprint)])
            [plumbing.core :refer (defnk fnk map-keys ?>)]
            (clojure.core [reducers :as r])
            [backtick :refer (template)]
            [clojure.java.io :as io :refer (input-stream output-stream)])
  (:import (net.fortuna.ical4j.data CalendarBuilder CalendarOutputter)
           (net.fortuna.ical4j.util UidGenerator)
           (net.fortuna.ical4j.model.property Attendee Version ProdId CalScale)
           (net.fortuna.ical4j.model.parameter Value Role Cn)
           (net.fortuna.ical4j.model Date Dur Calendar Property
                                     ComponentList PropertyList ParameterList
                                     TimeZoneRegistry TimeZoneRegistryFactory TimeZone)
           (net.fortuna.ical4j.model.component VTimeZone VEvent)
           (java.util.Calendar)
           (java.net URI)
           (java.util GregorianCalendar)
           (com.google.common.base CaseFormat)))

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

(import-static net.fortuna.ical4j.model.parameter.Role
               REQ_PARTICIPANT OPT_PARTICIPANT)


(import-static net.fortuna.ical4j.model.parameter.Value
               DATE)

;;; Predicates
(defn ics? [s] (= ".ics" (fs/extension s)))
(defn prodid? [x] (= (type x) ProdId))
(defn properties? [xs] (= (type xs) PropertyList))
(defn components? [xs] (= (type xs) ComponentList))

;;; schemadef
(s/defschema IcsFile (s/both (s/pred fs/file? 'fs/file?)
                             (s/pred ics? 'ics?)))
;; (s/validate IcsFile "/home/susy/Downloads/PayDay.ics")


;;;
;;; Schema Coercions
;;; (this is a complicated series of patterns)
;;;
(defn base-matcher [schema-pred value-pred value-fn]
  (fn [schema]
    (if (schema-pred schema)
      (fn [x]
        (if (value-pred x)
          (value-fn x))))))

(defn var-gets [x]
  (if (var? (resolve x)) (var-get (resolve x))
     (keyword (str x))))

(def bitmask
  "Functional composition of keyword name resolution (or string as it returns
  the same value) to symbol, resolved to static constant integer or, when not
  succeeded, returned as keyword unaltered. Extremely defensive but cool..."
  (comp var-gets symbol name))

(defn str-num-key? [x]
  "Predicate returns true if x is either a string, a number or a keyword"
  (or (string? x) (keyword? x) (number? x)))

(defn integerify
  "Tries to convert a string to an integer or returns default-value/0.
  This is useful in a few ways, e.g. when using db values for settings to
  fallback to as sanity/safety measure. Modified for use with numbers/keywords.
  If x is a keyword, will try to resolve that name in current scope or return
  string otherwise."
  ([x default-value]
   (try
     (if (or (nil? x) (empty? x))
       default-value
       (when (or (keyword? x) (string? x))
         (Integer/valueOf (name x))))
     (catch Exception e default-value)))
  ([x]
   (if (number? x) x
     (if (keyword? x) (bitmask x)
       (integerify x x)))))


(def m-key|int-key
  (base-matcher #(= (s/either s/Int s/Keyword) %)
                keyword? bitmask))

(def m-str-num-key|int-str
  (base-matcher #(= (s/either s/Int s/Str) %)
                str-num-key? integerify))

(def coerce-bitmask
  (sc/coercer {(s/either s/Int s/Keyword) (s/either s/Int s/Str)}
              (stc/or-matcher m-key|int-key m-str-num-key|int-str)))

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
  "Usage: (tz {}) ;=> #object"
  [{tzone :- s/Str "Europe/Amsterdam"}
   {tzreg :- TimeZoneRegistry (new-tz-registry)}]
  (-> tzreg (.getTimeZone tzone)))

;;;
;;; Java Native Calendar
;;;

(s/defn new-jcal :- GregorianCalendar
  "Create a new Java GregorianCalendar object. Partitions props by sequence of
  two items (key/value pairs) and loops over for side effect of adding the bit-
  flag and value to Calendar instance. Optional timezone defaults to obviously
  Europe/Amsterdam ^^"
  [props & tzone]
  (let [tzone (if-not (empty? tzone) (first tzone) (tz {}))
        inst (java.util.Calendar/getInstance tzone)]
    ; seq on a map creates , here [[k1 v1], [k2 v2], ...]
    (doseq [[k v] (-> props coerce-bitmask seq)]
      (.set inst k v)) ; side effect of calling calendar setter mutating object
    inst)) ; close over calendar instance

;;;
;;; iCal
;;;

;; from file
(s/defn read-calendar :- Calendar
  "Reads a .ics file from disk given a file path."
  [file-path :- IcsFile]
  (.build (CalendarBuilder.) (input-stream file-path)))

;; to file
(defn write-calendar [calendar foutput]
  (-> (CalendarOutputter.)
       (.output calendar (output-stream foutput))))

(def v2 Version/VERSION_2_0)
(def gregorian CalScale/GREGORIAN)

(defnk prodid [{company :- s/Str "ical4j"}
               {product :- s/Str "ical4c"}
               {version :- s/Str "1.0"} ;; product, not vcal vs ical?
               {locales :- s/Str "NL"}]
  (ProdId. (format "-//%s//%s %s//%s"
                   company product version locales)))


(defn normalize [sx]
  (map-keys (comp keyword lower-case)
            (into (sorted-map) sx)))

(defmacro hash-fn [kfn vfn x]
  "Abstracts the logic for mapping over a sequence and adding key/value pairs
  to a sorted-map with custom function k and v. Using suffix backtick for
  unquoting."
  (template (normalize (r/map (fn [z] (hash-map (~kfn` ~z) (~vfn` ~z)))
                              (seq ~x)))))


(defn add-property
  [x p] (-> x .getProperties (.add p)) x)

(defn get-property
  [x p] (-> x .getProperties (.getProperty p)))

(defn get-properties
  "Returns all properties from a calendar or component as a sorted-map
  of keywords and values. Being a bit careful on the schema with this one."
  [x] (->> x .getProperties (hash-fn .getName .getValue)))

(defn add-component
  [x c]
  (-> x .getComponents (.add c)) x)

(s/defn get-components
  "Returns all components from a given calendar as sorted-map keyword
  keys and their properties nested in the same fashion."
  [cal :- Calendar]
  (->> cal .getComponents (hash-fn .getName get-properties)))

(defn add-parameter
  [x v] (-> x .getParameters (.add v)))

(s/defn get-parameters
  "Get the parameters from a iCal property."
  [prop :- Property] (-> prop .getParameters))


(s/defn create-ical :- Calendar
  "Takes a optional seq of properties and returns a calendar object with those
  properties attached/set to it and returns the calendar."
  [& properties]
  (let [cal (Calendar.)]
    ;; side effects
    (doseq [p properties] (add-property cal p))
    cal))

(s/defn get-tz :- VTimeZone
  [cal :- GregorianCalendar] (-> cal .getTimeZone .getVTimeZone))

(s/defn get-tz-props :- {s/Keyword s/Str}
  [cal :- GregorianCalendar] (-> cal get-tz get-properties))

(defn get-tz-id [cal] (-> cal get-tz .getTimeZoneId))

;;;
;;; Duration
;;;

(s/defn new-duration :- Dur
  "Overloads which mimic those of ical4j Dur class constructor signatures."
  ([start :- Date, end :- Date] (Dur. start end))
  ([days :- s/Num, hrs :- s/Num, mins :- s/Num, secs :- s/Num]
   (Dur. days hrs mins secs)))

(s/defn negate-duration :- Dur
  [dur :- Dur] (.negate dur))

;; (-> (new-duration 2 12 30 0)
;;     str) ;=> "P2DT12H30M"

;;;
;;; vEvent
;;;

;;; temp!!!!
(def $pid "rob")
(defn generate-uid [] (-> $pid (UidGenerator.) .generateUid))

(s/defn new-event :- VEvent
  ; GregorianCalendar[java.util.Date] -> net.fortuna.ical4j.model.Date
  ([start, desc :- s/Str] (VEvent. start desc))
  ([start, end-or-dur, desc :- s/Str]
   (VEvent. start end-or-dur desc)))

;;;
;;; Attendees
;;;

;; new is the most literal (named parameters) kind
(defnk new-attendee :- Attendee
  [email :- s/Str
   {role :- Role req-participant}
   {cn :- s/Str "Participant"}]
  (let [amail (URI. (format "mailto:%s" email))
        cname (Cn. cn)
        attnd (Attendee. amail)]
    (add-parameter attnd role)
    (add-parameter attnd cname)
    attnd))

;; create is the positional parameters + sequence kind
(s/defn create-attendee
  ([attendees] (mapv #(apply create-attendee %) attendees))
  ([email role cn] (new-attendee {:email email :role role :cn cn})))

;; add actually adds to the main container object
(defn add-attendees
  "Runs the side-effect of adding attendees from sequence ax to an event ev.
  Finally returns the event object with properties mutated."
  [ev ax] (doseq [a ax] (add-property ev a)) ev)






;; demonstrate 2 methods of parsing dates/times
(let [cal (new-jcal {:year 2016 :month :july :day-of-month 3
                     :hour-of-day 14 :minute 35})
      tz (get-tz-props cal)
      event (let [event-name "New Years Dive"
                   ; use clj-time to generate some computed date coerced to java base
                   sd (-> (t/now) (t/plus (t/hours 122)) c/to-date Date.)
                   ; use custom native java sugar keyword to symbol resolution and coercion
                   e (new-jcal {:year 2016 :month :january :day-of-month 10
                                :hour-of-day 14 :minute 0 :second 0 :millisecond 0}) ; native java sugar
                   ed (-> e .getTime Date.) ; then extract the date
                   ev (->> event-name (new-event sd ed))
                   ;; add uid to event
                   ev1 (add-property ev (generate-uid))
                   ;; ev1 and ev now return the same object since addition is mutative
                   tze (get-tz-id e)
                   ;; add time zone info
                   ev2 (add-property ev tze)
                   ]
                   ev)
      attendees [(new-attendee {:email "rob@sevenmatches.com"
                                :cname "Developer 1"})
                 (new-attendee {:email "lars@sevenmatches.com"
                                :cname "Developer 2"})
                 ]
      attendees1 (doseq [a attendees]
                   (add-property event a))
      ]
  (print (.toString event))

  )

;; (-> (prodid {}) ; clean productid
;;     (create-ical v2 gregorian) ; create new ical

;; ;;     (add-component events)


;;     ))


;;      clojure.reflect/reflect
;;      :members
;;      (map :name)))



;; (s/defn expand :- {:properties (s/maybe [s/Any])
;;                    :components (s/maybe [s/Any])}
;;   "Takes a calendar and maybe returns properties and/or components in a clj kvs"
;;   [cal :- Calendar]
;;   {:properties (-> cal get-properties)
;;    :components (-> cal get-components)})



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


