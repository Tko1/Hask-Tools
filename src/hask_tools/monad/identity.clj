(ns hask-tools.monad.identity
  (:require [hask-tools.functor :refer :all]
            [hask-tools.monad.core :refer :all]))
(defrecord Identity [runIdentity])

(extend-type Identity
  Functor
  (fmap [fa f]
    (update fa :runIdentity f))
  
  Monad
  (>>= [m f]
    (f (:runIdentity m)))
  (>> [ma mb]
    (>>= ma (fn [_] mb)))
  (return [madt x]
    (Identity. x)))


(defn ->identity [x] (Identity. x))
