(ns mistletoe.component)

;;;; # Component Render

(defprotocol Render
  (render [self vdom]))
