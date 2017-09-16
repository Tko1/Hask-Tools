(ns hask-tools.functor)

(defprotocol Functor
  (fmap [fa f]
    "fmap :: fa -> (a -> b) -> fb"))

