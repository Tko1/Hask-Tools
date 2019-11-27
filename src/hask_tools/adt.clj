(ns hask-tools.adt
  (:require [hask-tools.debug :refer :all]
            [hask-tools.util :refer :all]
            [hask-tools.hsynt :refer :all]))

(defmacro defadt [-name & variants]
  (dlet [main-constr-name (symbol (str -name "."))
         gen-variant      (fn [variant]
                            (dlet
                             [vsym    (first variant)
                              vname   (symbol (str "-" vsym))
                              vconstr (symbol (str vname "."))
                              
                              
                              membersl (drop 1 variant)
                              membersv (into [] membersl)]
                             `((defrecord ~vname ~membersv)
                               (defn ~vsym ~membersv (~main-constr-name  ~(keyword vsym) (~vconstr ~@membersl))))))
         genned-variants   (map #(gen-variant %) variants)
         new-variants      (reduce concat genned-variants)]
        (prepend 'do
                 (concat
                  `((defrecord ~-name [~'vname ~'vrecord]))
                  new-variants))))



(defmacro data [ dname eq & args ]
  (dlet [a   args
         ors (horexpr args)
         variants (:or-exprs ors)
        ]
    `(defadt ~dname ~@variants))) 


;;;;;; Testing
(macroexpand '(defadt Animal (Cat) (Dog -name age location)))
(macroexpand '(defadt Animal (Kitty) (Doggy name age location)))
(defadt Animal (Kitty) (Doggy name age location))
(Doggy "Barky" 10 "Tennassy")
