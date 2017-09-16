(ns hask-tools.lens)
;;I am sure there is a way to fold these into one list?
(require '[clojure.string :as str])
(require '[hask-tools.debug :refer :all])
(require '[hask-tools.util :refer :all])
(require '[hask-tools.functor :refer :all])
(require '[hask-tools.monad.core :refer :all])
(require '[hask-tools.monad.const :refer :all])
(require '[hask-tools.monad.identity :refer :all])
(require '[hask-tools.adt :refer :all])

;;
;; Lens

;; setter :: part -> whole -> whole
(defncurry lens [getter setter]
  (fncurry [fa s]
    ((flip fmap)
     (partial (flip setter) s) 
     (fa (getter s)))))
;;Reverse lens;
;; setter :: whole -> part -> whole
(defncurry rlens [getter rsetter]
  (fncurry [fa s]
           ((flip fmap)
            (partial rsetter s)
            (fa (getter s)))))
;;This form of plens includes an update function
;; for polymorphic lens
(defmacro def-prlens [name getter rsetter]
  (let [ updater (symbol (str "update-" name)) ]
    `(do
       (defn ~updater [] (def ~name (rlens ~getter ~rsetter)))
       (def ~name (rlens ~getter ~rsetter)))))

(defncurry l-get [-lens obj]
  (:runConst (-lens #(->Const %) obj)))
(defncurry l-over [-lens f obj]
  (:runIdentity (-lens #(->Identity (f %)) obj)))
(defncurry l-set [-lens x obj]
  (l-over -lens (fn [_] x) obj)) 
(defn l-comp [& args]
    (fncurry [fa s]
             (((apply comp args)fa)s)))


(defn lens-record [& syms]
  (let [symsv  (vec syms)
        getter #(get-in % symsv)
        setter #(assoc-in % symsv %2)]
    (rlens getter setter)))
(defn lens-adt [sym]
  (l-comp (lens-record :vrecord) (lens-record sym)))

(def lens-map lens-record)
;; A reader for getin' and settin'
(defmacro lensread
  ([name]
  (let [words (str/split (str name) #"\.")
        obj   (symbol (first words))
        -lens (map symbol (rest words))]
    `(l-get (l-comp ~@-lens) ~obj)))

  ([name sym val]
   (dlet [ setequal?  (= sym '=)
         words (str/split (str name) #"\.")
         obj   (symbol (first words))
         -lens (map symbol (rest words))]
     (if setequal? `(l-over (l-comp ~@-lens) (fn [_#] ~val) ~obj)))))
(capitalize-first (name 'biRegister8))
;;;;; Generate lens
(defn gen-field-protocol [field-name]
  (let [ upper-field-name (capitalize-first (name field-name))
         protocol-name (symbol (str "Has" upper-field-name))
         getter-name   (symbol (str "get-" field-name))
         setter-name   (symbol (str "set-" field-name))]
  (eval `(defprotocol ~protocol-name
     (~getter-name [x#])
     (~setter-name [whole# part#])))))
(defn try-gen-field-protocol [field-name]
  (let [protocol-name  (symbol
                        (str "Has" (capitalize-first (name field-name))))]
    (if (not (resolve protocol-name))
      (gen-field-protocol field-name))))
;;;;;;;;;;;;;;;;
(defn gen-lens-protocol [field-name]
  (let [ upper-field-name (capitalize-first (name field-name))
         protocol-name (symbol (str "Has" upper-field-name))
         lens-name     (symbol (str "l" field-name))]
    (eval `(defmulti ~protocol-name
             (~lens-name [fa# s#])))))
(defn try-gen-lens-protocol [field-name]
  (let [protocol-name  (symbol
                        (str "Has" (capitalize-first (name field-name))))]
    (if (not (resolve protocol-name))
      (gen-lens-protocol field-name))))
;;;;;;;;;;;;;;;
(defn gen-rec-fields [rec field-name]
  (let [upper-field-name (capitalize-first (name field-name))
        type-name     rec
        protocol-name (symbol (str "Has" upper-field-name))
        getter-name   (symbol (str "get-" field-name))
        setter-name   (symbol (str "set-" field-name))
        ]
    
  (eval `(extend-protocol ~protocol-name
           ~type-name
           (~getter-name [x#]
            (~(keyword field-name) x#))
           (~setter-name [whole# part#]
            (assoc whole# ~(keyword field-name) part#))))))
(defn gen-rec-lens-field [rec field-name]
  (let [upper-field-name (capitalize-first (name field-name))
        type-name     rec
        protocol-name (symbol (str "Has" upper-field-name))
        lens-name     (symbol (str "l" field-name))
        ]
    
  (eval `(extend-protocol ~protocol-name
           ~type-name
           (~lens-name [fa#]
            (let [rsetter# (fn [x#] ~(keyword field-name) x#)
                  getter#  (fn [whole# part#] (assoc whole# ~(keyword field-name) part#))]
              (rlens getter# rsetter#)))))))
(defn gen-plens [field-name]
  (let [upper-field-name (capitalize-first (name field-name))
        protocol-name (symbol (str "Has" upper-field-name))
        getter-name   (symbol (str "get-" field-name))
        setter-name   (symbol (str "set-" field-name))

        lens-name     (symbol (str "l" field-name))]
    (eval `(def ~lens-name (rlens ~getter-name ~setter-name)))))
(defn get-record-constructor-arglen [ rec ]
  (-
    (-> rec .getConstructors first .getParameterTypes count)
    2))
(defn get-record-keys [rec]
  (dlet [rec-construct-name (symbol (str rec "."))
        rec-arglen         (get-record-constructor-arglen (eval rec))
        rec-construct-args (times nil rec-arglen)
        construct          (concat `(~rec-construct-name)
                                   rec-construct-args)]
        (keys (eval construct) )))
;;;
(defn make-lenses [rec]
  (doseq [i (vec (get-record-keys rec))]
    (domonad-env
     ->identity
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (field-name     <-  (symbol (name i)))
     (try-gen-field-protocol field-name)

     (gen-rec-fields rec field-name)
     (gen-plens          field-name))))

(defn make-plens [field-name]
  (try-gen-lens-protocol field-name))
(comment
(defn xxx [field-name type-name getter rsetter]
  (fmap (partial rsetter s) (fa (getter s)))))
(defn make-plenses [rec]
  (doseq [i (vec (get-record-keys rec))]
    (domonad-env
     ->identity
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (field-name     <-  (symbol (name i)))
     (make-plens field-name)

     (gen-rec-lens-field rec field-name))))
;;;;;;
(comment
(defrecord TestRecord100 [x y])
(make-plenses 'TestRecord100)

(defrecord TestRecord101 [x y]))
;;;;;;
;;Makes protocol and lens (I suppose sort of useless
;;since you'll have to redo the lens as more getters
;;and setters are added?  Oh right this was for a
;;non record  TODO fix
(defn make-field [name]
  (try-gen-field-protocol name)
  (gen-plens name))
           
(defn lnth [ind]
  (rlens #(nth % ind )
         #(sequence-conversion-fn (type %) (set-at ind %2 %))))
(def lfirst (lnth 0))
(def lsecond (lnth 1))
(defn lget-in [ syms ]
  (rlens #(get-in % syms)
         #(assoc-in % syms %2)))

;;(l-get (lget-in [:a :b :c]) { :a { :b {:c 14 :d 15} :c 24} :h 1 :f "cat"})

