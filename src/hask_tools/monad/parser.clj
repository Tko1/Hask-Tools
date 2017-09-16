(ns hask-tools.monad.parser)

(require '[hask-tools.monad.core :refer :all])
(require '[hask-tools.adt :refer :all])
(require '[hask-tools.lens :refer :all])
(require '[hask-tools.debug :refer :all])
(require '[clojure.spec.alpha :as s])
(require '[clojure.spec.gen.alpha :as gen])
(require '[hask-tools.util :refer :all])
(require '[hask-tools.functor :refer :all])
(require '[hask-tools.monoid :refer :all])
(s/def ::parsable #(or (coll? %) (string? %)))

(fn-spec-test ::parsable -> (s/coll-of (s/tuple ::all string?)))
(extend-protocol Monoid
  String
  (mappend [ma mb]
    { :pre [(= (type ma) (type mb))]}
    (str ma mb))
  (mempty [ma] "")
  
  clojure.lang.PersistentVector
  (mappend [ma mb]
    { :pre [(= (type ma) (type mb))]}
    (vec (concat ma mb)))
  (mempty [ma] [])

  clojure.lang.PersistentList
  (mappend [ma mb]
    { :pre [(or (= (type mb) clojure.lang.PersistentList)
                (= (type mb) clojure.lang.PersistentList$EmptyList))]}
    (into '() (reverse (concat ma mb))))
  (mempty [ma] '())
  
  clojure.lang.PersistentList$EmptyList
  (mappend [ma mb] 
  { :pre [(or (= (type mb) clojure.lang.PersistentList)
              (= (type mb) clojure.lang.PersistentList$EmptyList))]} mb)
  (mempty [ma] '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Parser [parse])
(defn fail-parse [x] [ nil x])
(defn parser-output-type [in out]
  "In:  The collection type to read from
        Examples: string? vector? (s/coll-of int)
   Out: What gets parsed out.."
  (s/tuple out in))
;;  (eval (concat '(s/tuple) `((fn [x#]
 ;;                              (or (= x# nil)
  ;;                                 (s/valid? ~out x#)))
   ;;                          ~in))))

(s/explain (parser-output-type char? char?) ["c" "dog"])
(s/explain (parser-output-type vector? char?) ["cad" [\c \a \d]])
(extend-protocol Functor
  Parser
  (fmap [fa f]
    (->Parser (fn [s]
                (let [ old-parse (:parse fa)
                      results   (old-parse s)
                      [res remain] results]
                  (if (= results (fail-parse remain))
                    (fail-parse remain)
                    [(f res) remain]))))))
(extend-protocol Monad
  Parser
  (>>= [m f]
    (->Parser (fn [s]
                (let [[u v] ((:parse m) s)
                      [x y] (if (= u nil)
                              (fail-parse s)
                              ((:parse (f u)) v))]
                  (if (= u nil)
                         (fail-parse s)
                         [x y] )))))
  (>> [ma mb]
    (>>= ma (fn [_] mb)))
  (return [mtype x]
    (->Parser (fn [s] [x s]))))
(def parser-type (->Parser (fn [s] [])))
(defn fail-parse [x] [ nil x])
(def fail-parser (->Parser (fn [s] (fail-parse s))))
(def string-type "")
;;;;;;; Constructors 
(defn new-Parser-a [spec parse]
  (->Parser
   (fn-spec
    `( ::parsable -> #(s/valid? (s/coll-of (s/tuple ~spec string?))))
    parse))) 
(defn new-Parser [parse]
  (new-Parser-a ::all parse))
;;;;;;;;; Return

(defn return-parser [ a ]
  (->Parser (fn [s] [a s])))
;;;;;;;;;
;;TODO used to be for osmethng else
(spec-defn coll-first [(::parsable coll)]
           (cond
             (string? coll)
             (first coll)

             (vector? coll)
             (first coll)

             :else
             (first coll)))
(spec-defn coll-rest [(::parsable coll)]
           (cond
             (string? coll)
             (reduce str "" (rest coll))

             (vector? coll)
             (vec (rest coll))))
;; TODO multimethod or protocol these.
(def coll-drop
  (curry-n
   2
   (fn [n coll]
     { :pre [(integer? n)
             (s/valid? ::parsable coll)
             (>= n 0)
             (<= n (count coll))]}
      (cond
        (string? coll)
        (.substring coll n)
        
        (vector? coll)
        (vec (drop n coll))

        :else
        (drop n coll)))))
;;;;;;;;;;;;;;;;;;
(defn optionp [pa pb]
  (->Parser (fn [s]
              (let [result ((:parse pa) s)
                    [res remain] result]
                (if (= result (fail-parse remain))
                  ((:parse pb) s)
                  result)))))


(defn unit [t]
  (->Parser (fn [s] [t s])))

;; Primitive parsers


(def headp 
  (->Parser (fn [s]
              (if (= s (mempty s))
                (fail-parse s)
                [(coll-first s) s]))))
(def consumep 
  (->Parser (fn [s]
              (if (= s (mempty s))
                (fail-parse s)
                [(mempty s) (coll-rest s)]))))
((:parse consumep) "c") 
(def itemp 
  (domonad
   (head <- headp)
   consumep
   (return-parser head)))
;;; Level 2
(defn predp [x]
  (->Parser (fn [s] (if (x (coll-first s))
                      ((:parse itemp) s)
                      (fail-parse s)))))
(defn specp [spec]
  (predp #(s/valid? spec %)))
(defn termp [x]
  (predp #(= % x)))

(defncurry parse-appendp [ to-monoid parsea parseb ]
  (domonad
   (a <- parsea)
   (b <- parseb)
   (return-parser (mappend (to-monoid a) (to-monoid b)))))
(def parse-str-appendp (parse-appendp str))

(def sequence-appendp
  (curry-n
   2
   (fn [ appender parsers ]
     { :pre [(s/valid? (s/coll-of #(= (type %) Parser)) parsers)]}
    (cond
      (empty? parsers)
      fail-parser
      
      (= 1 (count parsers))
      (first parsers)

      :else
      (appender (first parsers) (sequence-appendp appender (rest parsers)))))))
(def sequencep (sequence-appendp >>))
(defn stringp [ string ]
  (sequence-appendp parse-str-appendp (map termp string)))

(defn nums-to-stringp-test [ nums ]
  (sequence-appendp
   parse-str-appendp
   (map #(fmap (termp %) (fn [x] (str x))) nums)))
((:parse (parse-str-appendp itemp itemp)) "cat")
;;; Combine parsers

(defn termsp [ ts ]
  (sequencep (map #(termp %) ts)))


((:parse itemp) "")                        
((:parse (sequencep [ itemp itemp itemp itemp])) [0 100 12])

 
(defncurry add-parser-type [ type-in type-out parser]
  (->Parser (fn [s] { :pre [ (s/valid? type-in s)]
                     :post [ (s/valid? (parser-output-type type-in type-out) %)]}
              ((:parse parser) s))))
((:parse (add-parser-type vector? string? (termp "cat")))
 ["cat" 100 0 100 5])

;;((:parse (add-parser-type (s/coll-of integer?) string?)
 ;;        (fmap termp #(str 
        
