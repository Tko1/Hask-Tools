(ns hask-tools.monad.core
  (:require [hask-tools.debug :refer :all]
            [hask-tools.util :refer :all]))

(defprotocol Monad
  (>>= [m f]
    ">>= :: m a -> (a -> m b) -> m b")
  (>> [ma mb]
    ">>")
  (return [madt x]
    "return"))

(defmacro domonad [& args]
  { :pre [ (not (empty? args)) ] }
  ;; (a <- (Just 1))
  (cond
    (= 1 (count args))
    (first args)

    :else
    (dlet
        [next      (first args)
         is-bind?  (and (coll? next)
                        (= 3 (count next))
                        (= '<- (second next)))
         expr      (if is-bind?
                     `(>>= ~(nth next 2) (fn [~(first next)] ~(prepend 'domonad (drop 1 args))))
                      `(>> ~next ~(prepend 'domonad (drop 1 args))))]
      expr)))

(defmacro domonad-env [lifter & args]
  { :pre [ (not (empty? args)) ] }
  ;; (a <- (Just 1))
  (cond
    (= 1 (count args))
    (first args)

    :else
    (dlet
        [next      (first args)
         is-bind?  (and (= 3 (count next))
                        (= '<- (second next)))
         ni (if is-bind? (nth next 2))
         elifter   (eval lifter) 
         expr      (if is-bind?
                     `(>>= (~elifter ~ni) (fn [~(first next)] ~(prepend 'domonad-env lifter (drop 1 args))))
                      `(>> (~elifter ~next) ~(prepend 'domonad-env lifter (drop 1 args))))]
      expr)))

(defmulti lift (fn [t monad]
                 t))
