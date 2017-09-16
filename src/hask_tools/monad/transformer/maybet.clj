(ns hask-tools.monad.transformer.maybet)

(require '[hask-tools.monad.core :refer :all])
(require '[hask-tools.monad.maybe :refer :all])
(require '[hask-tools.functor :refer :all])
(defrecord MaybeT [ run-maybet ])
(extend-protocol Monad
  MaybeT
  (>>= [mt f]
    (->MaybeT
     (domonad
      (v <- (:run-maybet mt))
      (if (= v (Nothing))
        (:run-maybet mt)
        (:run-maybet (f (unmaybe v)))))))
  (>> [ma mb]
    (>>= ma (fn [_] mb)))
  (return [madt x]
    (->MaybeT (Just x))))
 
(defmethod lift MaybeT [t monad]
  (->MaybeT (fmap monad #(Just %))))
(defmethod lift :MaybeT [t monad]
  (->MaybeT (fmap monad #(Just %))))
