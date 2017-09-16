(ns hask-tools.monad.get)

(require '[hask-tools.monad.core :refer :all])
(require '[hask-tools.adt :refer :all])
(require '[hask-tools.monad.maybe :refer :all])
(require '[hask-tools.lens :refer :all])
(require '[hask-tools.util :refer :all])
(data Decoder =
      Fail bytestring error-text
      | Partial continue
      | Done bytestring x)
      ;;| BytesRead bytes-read  
(Done 12)

(data Get = GetC runCont)

(def get-word8
  (Get (fncurry [ bs succ ]
         (let [ w8 (first bs)]
           (succ (drop 1 bs) w8)))))
(defn run-get-word8-incremental [g]
  (Partial (fn [mbs]
             (if (= (Nothing) mbs)
               (Fail [] "Is this correct?")
               ((l-get (lens-adt :runCont) g)
                (unmaybe mbs)
                (fn [bs a] ;; TODO INCOMPLETE))))))
                  
(extend-protocol Monad
  Get
  (>>= [m f]
    (let [ old-cont (l-get (lens-adt :runCont) m)]
      
      (Getc (fncurry [i ks]
              (old-cont i (fncurry [i2 a]
                            ((l-get (lens-adt :runCont) (f a)) i2 ks)))))))

  (>> [ma mb]
    (>>= ma (fn [_] mb)))
  (return [madt x]
    (Just x))))
                  
(defncurry no-means-no [ r0 ]
  (let  [
         never-again (fn never-again [r]
                       (cond
                         (= :Done (:vname r))
                         r

                         (= :Partial (:vname r))
                         (let [ old-cont (l-get (lens-adt :continue) r)]
                          (never-again (old-cont (Nothing))))))
         go          (fn go [r]
                       (cond
                         (= :Done (:vname r))
                         r
                         
                         (= :Partial (:vname r))
                         (let [ old-cont (l-get (lens-adt :continue) r)]
                           (Partial (fn [ms]
                                      (cond
                                        (= :Just (:vname ms))
                                        (go (old-cont ms))

                                        (= (Nothing) ms)
                                        (never-again (old-cont ms))))))))]
    (go r0)))
(defncurry run-get-incremental [ g ]
  (Partial (fn [ms])
           
(defncurry run-get [ g lbs0 ]
         (let [ feed-all (fn feed-all [decoder lbs]
                           (cond
                             (= :Done (:vname decoder))
                             (l-get (lens-adt :x) decoder)

                             (= :Partial (:vname decoder))
                             (let [ cont (l-get (lens-adt :continue) decoder)]
                               (feed-all (cont (take 1 lbs)) (drop 1 lbs)))))] )(feed-all (run-get-incremental g) lbs0)) 
(defn ->maybe [x] (if (nil? x) (Nothing) (Just x)))
(drop 1 [1 2 3])
