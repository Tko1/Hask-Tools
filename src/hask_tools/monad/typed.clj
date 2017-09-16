(ns hask-tools.monad.typed)

(defrecord Typed [contract x])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WIP
;;
;; Won't allow x to break contract.
;; However, what happens when x tries to break contract?
;; Options: Allows break, disallows break, and (anything else)
;;          What if we encode the action too?
;; Typed (action contract x)
;; Typed ( mod-bind   (> 0) -1))?
;; A monad that maps something to different binds!
;;                    the value
;; A maybe is this type in a sense.  It is based on
;; not on the x, but o
;;(Just 1 = Many ( (fn [ma mb] (if :t ma = Just) Just, Nothing)
;;bind =                  
(extend-protocol Monad
  Typed
  (>>= [m f]
    (if (= m (Nothing2))
      m
      (f (l-get (lens-adt :x) m))))
  (>> [ma mb]
    (>>= ma (fn [_] mb)))
  (return [madt x]
    (Just2 x)))
