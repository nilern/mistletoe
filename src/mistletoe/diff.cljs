(ns mistletoe.diff
  (:require [clojure.string :as str]
            [goog.object :as obj]
            [mistletoe.node :as node]
            [mistletoe.component :refer [render]]
            [mistletoe.dependency :refer [resolve]]
            [mistletoe.updates :refer [apply-delta! deref-delta! AppendChild RemoveChild ReplaceChild
                                       schedule-set-property! schedule-set-style-property!
                                       schedule-set-event-handler!]]))

;;; OPTIMIZE: Use iterator and requestIdleCallback to enable render scheduling.

;;;; # VDOM Diffing

(declare diff-subtrees!)

;; OPTIMIZE: If dependency can be resolved, use the resolved value immediately:
(defn diff! [deltas parent-dom prev-vdom new-vdom]
  (cond
    (undefined? prev-vdom) (do (node/materialize! new-vdom parent-dom deltas)
                               (.push deltas (AppendChild. parent-dom (.-dom new-vdom)))
                               new-vdom)

    (undefined? new-vdom) (do (.push deltas (RemoveChild. parent-dom (.-dom prev-vdom)))
                              nil)

    (not= (.-nodeName prev-vdom) (.-nodeName new-vdom))
    (do (node/materialize! new-vdom parent-dom deltas)
        (.push deltas (ReplaceChild. parent-dom (.-dom new-vdom) (.-dom prev-vdom)))
        new-vdom)

    (string? (.-nodeName prev-vdom)) (do (set! (.-dom new-vdom) (.-dom prev-vdom))
                                         (diff-subtrees! deltas (.-dom prev-vdom) prev-vdom
                                                         new-vdom)
                                         new-vdom)

    :else (let [element (.-childNode prev-vdom)
                element* (render (.-component prev-vdom) new-vdom)]
            (set! (.-childNode prev-vdom) element*)
            (node/set-parents! prev-vdom)
            (diff! deltas parent-dom element element*)
            prev-vdom)))

(declare diff-attributes! diff-children!)

(defn- diff-subtrees! [deltas dom prev-vdom new-vdom]
  (diff-attributes! deltas prev-vdom new-vdom)
  (diff-children! deltas dom prev-vdom new-vdom))

;; OPTIMIZE: If dependency can be resolved, use the resolved value immediately:
(defn- diff-attributes! [deltas prev-vdom new-vdom]
  (obj/forEach new-vdom (fn [v k _]
                          (case k
                            ("dom" "parentNode" "childNodes" "nodeName") nil

                            "style"
                            (obj/forEach v (fn [v k _]
                                             (when (not= v (aget (.-style prev-vdom) k))
                                               (schedule-set-style-property! v k new-vdom deltas))))

                            (when (not= v (aget prev-vdom k))
                              (if (str/starts-with? k "on")
                                (schedule-set-event-handler! v (subs k 2) new-vdom deltas
                                                             (aget prev-vdom k))
                                (schedule-set-property! v k new-vdom deltas)))))))

;; OPTIMIZE: Use keys to make reordering children efficient:
(defn- diff-children! [deltas dom prev-vdom new-vdom]
  (if (string? (.-nodeName prev-vdom))
    (when-not (= (.-nodeName prev-vdom) "#text")
      (let [prev-children (.-childNodes prev-vdom)
            new-children (.-childNodes new-vdom)]
        (dotimes [i (max (alength prev-children) (alength new-children))]
          (diff! deltas dom (aget prev-children i) (aget new-children i)))))
    (diff! deltas dom (.-childNode prev-vdom) (.-childNode new-vdom))))

;;;; # Flushing Updates to DOM

(defn apply-diff! [deltas]
  (dotimes [i (alength deltas)]
    (deref-delta! (aget deltas i)))

  (dotimes [i (alength deltas)]
    (apply-delta! (aget deltas i))))
