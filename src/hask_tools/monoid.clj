(ns hask-tools.monoid)

(defprotocol Monoid
  (mappend [ma mb])
  (mempty [ma]))
