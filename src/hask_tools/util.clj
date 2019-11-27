(ns hask-tools.util
  (:require [hask-tools.debug :refer :all]
            [clojure.spec.alpha :as s]))

(defn arities [v]
  (->> v meta :arglists (map count)))
(defn arity [f]
  (-> f class .getDeclaredMethods first .getParameterTypes alength))

(defn type? [a b] (= (type b) a))
(defn ->assert [t x] (assert (t x)) x)

;;;
(defn id [x] x)
;;;
(defn flip
  ([f] ;;((flip str) "END" "Begin") => BeginEND
   (fn
    ([] (f))
    ([a1] (f a1))
    ([a1 a2] (f a2 a1))
     ([a1 a2 & as] (apply f (concat `(~a2 ~a1) as)))))
  ([f & args] (apply (flip f) args))) ;; (flip str "END" "Begin") => BeginEnd
(assert (= ((flip str) "END" "Begin") "BeginEND"))
(assert (= ((flip str) "END" "Begin" "EndEnd") "BeginENDEndEnd"))
(assert (= (flip str "END" "Begin") "BeginEND"))
;;;
(comment
(defn full-flip [f]
  (fn
    ([] (f))
    ([a1] (f a1))
    ([a1 a2] (f a2 a1))
    ([a1 a2 & as] (apply f (concat (reverse as) `(~a2 ~a1))))))
(assert (= 4 ((full-flip /) 2 2 2 32)))
(assert (= "ThisIsBackwards" ((full-flip str) "Backwards" "Is" "This"))))
;;;
(def pipe
  (full-flip comp))
((comp (partial * 2) second first) [[1,6] , 50])
((pipe first second (partial * 2)) [[1,6] , 50])
(def |> pipe)
;;;
;;;;;;;;;;; Currying
(defn curry-n [n f]
  (with-meta
    (fn [ & args]
      (let [argc (count args)]
        (if (< argc n)
          (curry-n (- n argc) (apply partial f args))
          (apply f args))))
    {:argc n}))

(defmacro fncurry [args & body]
  (let [arg-count (count args)
        curry-count arg-count]
       ;; orig-fn     (fn args body)]
    `(curry-n ~curry-count (fn ~args ~(cons 'do body)))))

(defmacro defncurry [fname args & body]
  `(def ~fname ~(conj body args 'fncurry)))
(macroexpand '(defncurry add [a b] (+ a b)))


(defn comp-curr [a b]
  (let
      [ argc (:argc (meta b))]
    (curry-n argc (comp a b))))
;;;;;;;;;;;;; Collections
(defncurry indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(def first-index-of (comp-curr first indices-of))

(defprotocol HasList
  (get-list [x])
  (set-list [whole part]))

;; To lists
(defn lazy-to-list [ls]
  {:pre [(type? clojure.lang.LazySeq ls)]
   
   :post [(list? %)]}
  (into '() (reverse ls)))
(defncurry set-from-to [n m newls ls]
  
     (dlet
         [fhalf (first (split-at n ls))
          shalf (second (split-at (+ m 1) ls))
          middle newls]
         (lazy-to-list (concat fhalf middle shalf))))

(defncurry set-at [n val ls]
  (set-from-to n n (list val) ls))

(defn vec-to-list [ls]
  (->assert list? 
            (into '() (reverse (->assert vector? ls)))))
(defn array-seq-to-list [ls]
  {:pre [(type? clojure.lang.ArraySeq ls)]
   
   :post [(list? %)]}
  (into '() (reverse ls)))
(extend-protocol HasList
  clojure.lang.LazySeq
  (get-list [ls] (lazy-to-list ls))
  (set-list [whole part] (lazy-to-list part))

  clojure.lang.PersistentVector
  (get-list [vs] (vec-to-list vs))
  (set-list [whole part] (vec-to-list part))

  clojure.lang.ArraySeq
  (get-list [ls] (array-seq-to-list ls))
  (set-list [_ part] (array-seq-to-list part))
  
  clojure.lang.PersistentList
  (get-list [ls] ls)
  (set-list [_ part] part))
;;;;
(defn list-to-vec [ls]
  (->assert vector?
            (vec (->assert list? ls))))
(defn lazy-to-vec [ls]
  {:pre [(type? clojure.lang.LazySeq ls)]
   
   :post [(vector? %)]}
  (vec ls))
(defn array-seq-to-vec [ls]
  {:pre [(type? clojure.lang.ArraySeq ls)]
   
   :post [(vector? %)]}
  (into '[] (reverse ls)))
(defprotocol HasVector
  (get-vector [vs])
  (set-vector [whole part]))
(extend-protocol HasVector
  clojure.lang.LazySeq
  (get-vector [ls] (lazy-to-vec ls))
  (set-vector [whole part] (lazy-to-vec part))

  clojure.lang.ArraySeq
  (get-vector [ls] (array-seq-to-vec ls))
  (set-vector [whole part] (array-seq-to-vec part))
  
  clojure.lang.PersistentVector
  (get-vector [vs]  vs)
  (set-vector [whole part] part)

  clojure.lang.PersistentList
  (get-vector [ls] (list-to-vec ls))
  (set-vector [_ part] (vec-to-list part)))
(defmulti sequence-conversion-fn (fn [a b] a))
(defmethod sequence-conversion-fn
  clojure.lang.PersistentList
  [a b]
  (get-list b))
(defmethod sequence-conversion-fn clojure.lang.PersistentVector
  [a b]
  (get-vector b))
;;(defmethod sequence-conversion-fn [clojure.lang.
(defn prepend  [& args]
  (let [ coll (last args)
         vs   (vec (drop-last 1 args))]
  (if (vector? coll)
    (into vs coll)
    (into coll (reverse vs)))))
(prepend 1 '(2 3 4))
(prepend 1 [2 3 4])
;;;;;;;;;;;;;;
(defn postpend [v coll]
  (cond
    (vector? coll)
    (into coll [v])
    ;; List, hopefully
    :else
    (concat coll `(~v))))
(assert (= '(1 2 3 4) (postpend 4 '(1 2 3))))
(assert (= [1 2 3 4]  (postpend 4 [1 2 3])))
;;;;;;;;;;;;;;;
(defn pop-head [coll]
  (if (vector? coll)
    (subvec coll 1)
    ;;Else list or chaos
    (pop coll)))
(def peek-head first)
(defn peek-and-pop-head! [a]
  (let [ head (peek-head @a) ]
    (swap! a pop-head)
    head))
(defn pop-last [coll]
  ;;TODO find out the time complexity of the different coll
  ;; and their operations
  ((if (vector? coll)
     vec
     id)
   (butlast coll)))
(list? (prepend 1 '(2 3 4)))



(defn times [x n]
  (cond (< n 1)
        nil
        (= n 1)
        `(~x)
        :else
        (lazy-to-list (concat `(~x) (times x (- n 1))))))


(defn subcoll-from-to [ from-type to-type converter ]
  (curry-n
   3
   (fn [n m ls]
     { :pre  [(type? from-type ls)
              (integer? n)
              (integer? m)
              (and  (>= n 0)
                    (<  n (count ls)))
              (and  (>= m 0)
                    (<  m (count ls))
                    (>= m n))]
      :post [(type? to-type %)]} 
     (let
         [new-len (- m n -1)
          dropc   n
          takec   new-len]
       (converter (take takec (drop dropc ls)))))))
(def subvec-test
  (subcoll-from-to
   clojure.lang.PersistentVector
   clojure.lang.PersistentVector
   vec)) 
(def sublist
  (curry-n
   3
   (fn
     [n m ls]
     {
      :pre 
      [(list?    ls)
       (integer? n)
       (integer? m)
       (and  (>= n 0)
             (<  n (count ls)))
       (and  (>= m 0)
             (<  m (count ls))
             (>= m n))]
      :post 
      [(list?  %)]
      }
     (let
         [new-len (- m n -1)
          dropc   n
           takec   new-len]
       (lazy-to-list (take takec (drop dropc ls)))))))
(def subvector
  (subcoll-from-to
   clojure.lang.PersistentVector
   clojure.lang.PersistentVector
   vec))

(defmulti subcoll (fn [a b c] (type c)))
(defmethod subcoll clojure.lang.PersistentVector
  [n m ls]
  (subvector n m ls))
(defmethod subcoll clojure.lang.PersistentList
  [n m ls]
  (sublist n m ls))
(defn partition-indices [ indices ls ]
  ;;'(0 1 3 6 9)
  (partition-by #(some (fn [x] (= x  (.indexOf ls %))) indices) ls))
(defn remove-every-other [bool coll]
  (cond
    (empty? coll)
    coll
    
    bool
    (prepend (first coll) (remove-every-other (not bool) (drop 1 coll)))

    :else
    (remove-every-other (not  bool) (drop 1 coll))))
(remove-every-other true '(1 2 3 4 5))

(defn is-pairs? [ps]
  (every? (fn [x] (and (coll? x) (= (count x) 2))) ps))


(defn all? [preds x]
  (if (empty? preds)
    true
    (and ((first preds) x) (all? (rest preds) x))))
;;;;;;;;;;;;;;;;;;;;;;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn capitalize-n [text m n]
  (clojure.string/replace
   text
   (re-pattern (str "^(.{" m "})(.{" (- n m -1) "})")) 
   (fn [[_ bef after]] (str bef (clojure.string/upper-case after)))))
(defn capitalize-first [text]
  (capitalize-n text 0 0))
;;;;;;;;;;;;;;;;;;;;;;;; Math ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bitmask [n m]
  (let [
        [-n,-m]    (if (< m n)
                     [m n]
                     [n m])
        --n        (if (< (- -n 1) 0)
                     0
                     (- -n 1))
        len        (- m --n)
        nonshifted (- (bit-shift-left 1 len) 1)
        shifted    (bit-shift-left nonshifted --n)]
    shifted))

(defn word8-pair-to-word16 [[worda wordb]]
  (bit-or
    (bit-shift-left wordb 8)
    worda))

(defn word16-to-word8-pair [ word16]
  (let [ wordb (bit-shift-right (bit-and word16 (bitmask 9 16)) 8)
        worda (bit-and word16 (bitmask 0 8))]
    [worda wordb]))
;;;; Some type stuff
(defmacro defenum [ -name & enums ]
  `(s/def ~-name #(some #{%} ~(vec enums))))

;;;; new typed defun stuff

(defn is-pairs-or-pred? [ps]
  (or
   (some   (fn [x] (= :pre (first x))) ps)
   (is-pairs? ps)))
(defmacro spec-fn [args & body]
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
                              [(postpend '% `(s/valid? ~retval-type))]
                              [])
         mod-body           (if is-retval?
                              (rest body)
                              body)
         type-checks        (map (partial prepend 's/valid?) args)
         base-args          (into [] (map last args))
         base-args-list     (into '() (reverse base-args))
         vec-of-type-checks (into [] type-checks)
         do-body            (conj mod-body 'do)]
                     
    `(fn ~base-args
       { :pre ~vec-of-type-checks
         :post ~retval-check}
       ~do-body)))
(defmacro spec-defn [name args & body]
  `(def ~name (spec-fn ~args ~@body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro spec-defrecord [name args]
  (let [constructor-name (symbol (str name "."))
        fn-name           (symbol (str "new-" name))
            
        base-args          (into [] (map last args))
        base-args-list     (into '() (reverse base-args))
        
        constructor-name   (symbol (str "->" name))
        called-constructor (conj base-args-list constructor-name)]
    `(do
       (defrecord ~name ~base-args)
       (spec-defn ~fn-name ~args ~called-constructor))))
;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::all (fn [x] true))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::fn-sig (s/and
                 (s/coll-of #(or (= % '->)(fn? (eval %))(keyword? %)))
                 #(every? (fn [x] (= x '->)) (remove-every-other false %))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fn-spec [ fn-sig f ]
  { :pre [ (s/valid? ::fn-sig fn-sig)] }
  (let [specs-syms (remove-every-other true fn-sig)
        specs      (eval (vec specs-syms))
        arg-specs      (vec (drop-last 1 specs)) 
        retval-spec    (last specs)]
    (fn [ & args] { :pre [ (reduce
                            #(and % %2)
                            (map s/valid? arg-specs args)) ]
                    :post [ (s/valid? retval-spec %)]}
      (apply f args))))
;;

(def add-ints
  (fn-spec
   '( int? -> int? -> ::all)
   (fn [a b]
     (+ a b) "cat")))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn-spec-test [ & args ]
  (s/explain ::fn-sig args))
;;
;;(fn-spec-test ::a -> ::b -> ::c -> ::d)
;;(fn-spec-test ::a -> ::b -> #(= % 1) -> ::d)
;;
