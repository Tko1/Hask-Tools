(ns hask-tools.monad.maybe)

(require '[hask-tools.monad.core :refer :all])
(require '[hask-tools.adt :refer :all])
(require '[hask-tools.lens :refer :all])
(data Maybe = Just x | Nothing)

(extend-protocol Monad
  Maybe
  (>>= [m f]
    (if (= m (Nothing))
      m
      (f (l-get (lens-adt :x) m))))
  (>> [ma mb]
    (>>= ma (fn [_] mb)))
  (return [madt x]
    (Just x))) 
(defn ->maybe [x] (if (nil? x) (Nothing) (Just x)))

(defn unmaybe [x] (if (= x (Nothing)) nil (l-get (lens-adt :x) x)))
