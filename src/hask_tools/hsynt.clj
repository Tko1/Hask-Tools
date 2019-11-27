(ns hask-tools.hsynt
  (:require [hask-tools.debug :refer :all]
            [hask-tools.util :refer :all]))
;; Converts haskell  a b $ c d -> (a b (c d))
(defmacro heval$ [& args]
   (dlet
    [dollars        (indices-of #(= % '$) args)
     adjust-dollars (fn
                      f
                      [inds old-args]
                      (if(empty? inds)
                        old-args
                        (dlet
                         [
                          i                inds
                          o                old-args
                          last-ind         (- (count args) 1)
                          last-dollar      (last inds)
                          [same,changed]   (split-at (+ 1 last-dollar) old-args)
                          cleaned-same     (drop-last 1 same)
                          cleaned-changed  changed
                          
                          new-inds         (drop-last 1 inds)
                          new-args         (concat
                                            cleaned-same
                                            `(~cleaned-changed))]
                         
                         (f new-inds new-args))))]
    `~(adjust-dollars dollars args)))

;;;;;;;;
;; Allows (hdef test a = + a $ + 1 2)
(defmacro hdef [& args]
  (dlet [equals         (first-index-of #(= % '=) args)

         last-ind       (- equals 1)
         first-body-ind (+ equals 1)
         last-body-ind  (- (count args) 1)
         t              (type args)
         list-args      args ;; (array-seq-to-list args)
         fn-sig   (sublist
                   0
                   last-ind
                   list-args);;(array-seq-to-list args))
         is-def?  (= 1 (count fn-sig))
         is-defn? (not is-def?)
         fn-def   (cond
                    is-def? '(def)
                    is-defn? '(defncurry)
                    :else
                    (println "Error hdef; should be def or defn"))
         fn-name  (first fn-sig)
         fn-args  (if is-defn?
                    `(~(list-to-vec
                     (sublist 1 last-ind fn-sig)))
                    '())
         
         fn-body   (sublist
                   first-body-ind
                   last-body-ind
                   list-args)

         def      (concat fn-def
                          `(~fn-name)
                          fn-args
                          `(~(concat '(heval$) fn-body)))
        ]
        def))
;; Testing
(macroexpand '(hdef add x y = + x y))
(hdef add x y = + x y)  
(heval$ add 0 $ add 1 $ add 1 2 )
(heval$ add 1 2)
(macroexpand '(heval add $ add 1 $ add 1 2))
(hdef add x y = do (+ x y)) 
(hdef add2 x y = + x y)
(hdef add x y z z2 = add2 x $ add2 y $ add2 z z2)

;;;;
(defmacro hlet-unit [& args]
  (dlet
      [ equals-ind (first-index-of #(= % '=) args)

       
       last-ind       (- equals-ind 1)
       first-body-ind (+ equals-ind 1)
       last-body-ind  (- (count args) 1)
       t              (type args)
       list-args      args ;; (array-seq-to-list-args
       fn-sig   (sublist
                 0
                 last-ind
                 list-args)
       is-def?  (= 1 (count fn-sig))
       is-defn? (not is-def?)
       fn-def   (cond
                  is-def? '()
                  is-defn? '(fn)
                  :else
                  (println "Error hdef; should be def or defn"))
       var-name  (first fn-sig)
       fn-args  (if is-defn?
                  `(~(list-to-vec
                      (sublist 1 last-ind fn-sig)))
                  '())
         
                    
       fn-body (concat
                '(heval$)
                (sublist
                 first-body-ind
                 last-body-ind
                 list-args))
       
       def    (if is-defn?
                (concat
                 '(fn)
                 fn-args
                 `(~fn-body))
                fn-body)
       binding  [(conj '() var-name 'quote ) def]]
      binding))

(macroexpand '(hlet-unit a = + 1 $ + 2 2))
(hlet-unit a = + 1 $ + 2 2)
(macroexpand '(hlet-unit a x = + 1 $ + 2 2))

(defmacro tlet [bindings body]
  (dlet
   [b bindings
    unit-bindings (map #(->> %
                             (prepend 'hlet-unit)
                             eval)
                       bindings)
    vbindings     (reduce into unit-bindings)]
   
   `(let ~vbindings ~body)))
(tlet [
       (a x = + 1 $ + 2 x)
       (b x = + 1 2)]
      (a (b 2)))
;;(macroexpand '(tlet (hlet-unit a x y = + 1 $ + 2 2) a))
(macroexpand '(hlet-unit a x y = + 1 $ + 2 7))
(macroexpand '(heval$ fn [x y] + 1 $ + x 2))
(comment
(defmacro hlet [& args]
  (let
      [ in-indx        (indices-of #(= % 'in) args)
       [bind,body]    (split-at in-indx args)
       splitbind      (partition 3 bind)
       drop=          (map #( ))
       cleanedbind    bind
       cleanedbody    (drop 1 body)])))
(comment       
(hdef tokenize pred list =
      let
          [ a 1
            b 2 ]
          b))
            

  
  ;;a b c = d e f g
;(defmacro test-macro [ & rest]
;  (index-of rest '=))
;;(hask
;; +1 x = + x 1bhg  ;









(hdef   hmap f coll = map f coll )
(hdef   add  x y = + x y)
(hdef   addArr n = hmap $ add n)
(heval$ (addArr 3) '(4 4 4))


(defn horexpr [args]
      (let
          [  ors     (indices-of #(= % '|) args)
           pargs+|   (partition-indices ors args)
           pargs     (remove-every-other true pargs+|)
           pargsv    (into [] pargs)]
        `{:or-exprs ~pargsv}))
(defmacro hequalexpr [& args]
  (let
      [equals   (first-index-of #(= % '=) args)
       last-ind (- equals 1)
       first-body-ind (+ equals 1)
       last-body-ind  (- (count args) 1)
       
       argsl   args;;(array-seq-to-list args)
       fhalf   (sublist 0 last-ind argsl)
       shalf   (sublist first-body-ind last-body-ind argsl)

       exprname (first fhalf)
       exprargs (drop 1 fhalf)

       

       ]
    `{:expr-name '~exprname
     :expr-args '~exprargs
      :expr-body '~shalf }))


(macroexpand '(hequalexpr + b c = + b c))
(macroexpand (horexpr '(Cat | Dog | Chicken a b c)))

(horexpr '(Catt | DogT | ChickenT a b c))
(hequalexpr katy a b = + a b c)
