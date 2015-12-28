(ns ical4c.macros
  (:require (clojure [set :refer (intersection)])
            [backtick :refer (template)])
  (:import (com.google.common.base CaseFormat)))

(def ^:private uu->lh
  #(-> CaseFormat/UPPER_UNDERSCORE (.to CaseFormat/LOWER_HYPHEN %)))

(def ^:private lh->uu
  #(-> CaseFormat/LOWER_HYPHEN (.to CaseFormat/UPPER_UNDERSCORE %)))

(def ^:private lc->lh
  #(-> CaseFormat/LOWER_CAMEL (.to CaseFormat/LOWER_HYPHEN %)))

(def ^:private kw->sym
  (comp symbol lh->uu name))

(def ^:private kws->syms
  #(mapv kw->sym %))

(defmacro import-static
  "Courtesy of Stuart Sierra with some alterations by Rob Jentzema

  Imports the named static fields and/or static methods of the class as
  (private) symbols in the current namespace. Returns a var in case of static
  fields and macro to call static method, so no first class citizens - remember.
  Name conversion from camelCase to lispy-names is performed. Class names must
  be fully qualified as you would with import in the regular fashion.

  Example:
      user=> (import-static java.lang.Math PI sqrt)
      user=> pi
      3.141592653589793
      user=> (import-static java.util.UUID randomUUID
      user=> (random-uuid)
      #uuid 1f26d25e-6692-47e0-953d-04690f388910"

  [class & fields-and-methods]
  (let [only (set (map str fields-and-methods))

        fqn (-> (resolve class) .getName) ; this isn't always possible/wanted because it requires a (:import ...)
        the-class (. Class forName (str class))
        static? (fn [x]
                    (. java.lang.reflect.Modifier
                       (isStatic (. x (getModifiers)))))
        statics (fn [array]
                    (set (map (memfn getName)
                              (filter static? array))))
        all-fields (statics (. the-class (getFields)))
        all-methods (statics (. the-class (getMethods)))
        fields-to-do (intersection all-fields only)
        methods-to-do (intersection all-methods only)
        make-sym (fn [string f]
                     (with-meta (symbol (f string)) {:private true}))
        import-field (fn [name]
                         (list 'def (make-sym name uu->lh)
                               (list '. class (symbol name))))
        import-method (fn [name]
                          (list 'defmacro (make-sym name lc->lh)
                                '[& args]
                                (list 'list ''. (list 'quote class)
                                      (list 'apply 'list
                                            (list 'quote (symbol name))
                                            'args))))]
    `(do ~@(map import-field fields-to-do)
         ~@(map import-method methods-to-do))))


;; (defmacro import-static-by-keys [jns & forms]
;;   (let [xs (partition 2 forms)
;;         m (into (sorted-map) (map #(apply hash-map %) xs))
;;         ks (keys m)
;;         vs (kws->syms (flatten (vals m)))]
;;     (template
;;       (import-static ~jns
;;                      ~@(vs)))))
