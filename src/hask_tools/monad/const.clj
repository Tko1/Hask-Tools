(ns hask-tools.monad.const
  (:require [hask-tools.functor :refer :all]
            [hask-tools.monad.core :refer :all]))

(defrecord Const [runConst])
(extend-type Const
  Functor
  (fmap [fa f]
    fa))
