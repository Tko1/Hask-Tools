(ns hask-tools.typed)

(require '[hask-tools.util])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO Remove? Pretty sure I replaced this with a spec version
;; of type checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require '[hask-tools.debug])

;;(defn type? [a b] (= (type b) a))
;; Moved to util
;;(defn ->assert [t x] (assert (t x)) x)

(def List clojure.lang.PersistentList)
(def Vector clojure.lang.PersistentVector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Creates ..statically typed args
;;
;; TODO make one that takes (pred arg) , from which you can build
;;      type predicates?
;; TODO typed-let
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-pairs-or-pred? [ps]
  (or
   (some   (fn [x] (= :pre (first x))) ps)
   (is-pairs? ps)))
(defmacro typed-fn [args & body]
  { :pre
   [(is-pairs-or-pred? args)]
  }
  (dlet [
         argc               (count args)
         args               args
         is-retval?         (and
                             (list? (first body))
                             (= '-> ((comp first first) body)))
         retval-type        (if is-retval?
                              ((comp second first) body))
         retval-check       (if is-retval?
                              [(postpend '% `(type? ~retval-type))]
                              [])
         mod-body           (if is-retval?
                              (rest body)
                              body)
         type-checks        (map (partial prepend 'type?) args)
         base-args          (into [] (map last args))
         base-args-list     (into '() (reverse base-args))
         vec-of-type-checks (into [] type-checks)
         do-body            (conj mod-body 'do)]
                     
    `(fn ~base-args
       { :pre ~vec-of-type-checks
         :post ~retval-check}
       ~do-body)))
(defmacro typed-defn [name args & body]
 `(def ~name ~(concat `(typed-fn ~args) body)))
;; Tests
(typed-defn t-add [(Long a) (Long b)] (+ a b)) 
(assert (= (t-add 1 5) 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Typed defrecord
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro typed-defrecord [name args]
  (let [constructor-name (symbol (str name "."))
        fn-name           (symbol (str "new-" name))
            
        base-args          (into [] (map last args))
        base-args-list     (into '() (reverse base-args))
        
        constructor-name   (symbol (str name "."))
        called-constructor (conj base-args-list constructor-name)]
    `(do
       (defrecord ~name ~base-args)
       (typed-defn ~fn-name ~args ~called-constructor))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
