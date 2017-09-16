(ns hask-tools.debug)
  ;;(:require [hask-tools.util :refer [full-flip]]))

;;(require '[hask-tools.util :refer [full-flip]])
;; Copied from util so that hask-tools.debug has no
;; dependencies
;;;;;;;;;;;;;;;;;;;; Util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn full-flip [f]
  (fn
    ([] (f))
    ([a1] (f a1))
    ([a1 a2] (f a2 a1))
    ([a1 a2 & as] (apply f (concat (reverse as) `(~a2 ~a1))))))
(assert (= 4 ((full-flip /) 2 2 2 32)))
(assert (= "ThisIsBackwards" ((full-flip str) "Backwards" "Is" "This")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def +enable-debug-print?+ false)
(def +enabled-debug-ids+ '())
(defn nothing [ & rest])
(def dprintln (if +enable-debug-print?+ println nothing))

;; The  body of this function
;; is taken from http://brownsofa.org/blog/2014/08/03/debugging-in-clojure-tools/
(defmacro d?let [debug? bindings & body]
  (let [ printfn (if debug? println nothing) ] ;;if debug? 
  `(let [~@(mapcat (fn [[n v]]
                     (if (or (vector? n) (map? n))
                       [n v]
                       [n v '_ `(~printfn (name '~n) ":" ~v)]))
                     (partition 2 bindings))]
     ~@body)))
;; Prints debug info for let if n is a valid debug id
(defmacro dlet-n [n bindings & body]
  ((full-flip conj)
    'd?let
    (some #{n} +enabled-debug-ids+) ;; Debug?
    bindings
    body))

;; Prints if global debug var is set
(defmacro dlet [bindings & body]
  ((full-flip conj)
     'd?let +enable-debug-print?+ bindings body)) 

