(ns mistletoe-deck.core
  (:require [mistletoe.signal :as sgn :refer [smap]]
            [mistletoe.signal.util :refer [map-index-cached]]
            [mistletoe.dom :refer [el append-child!]]))

(defn- rank-symbol [rank]
  (case rank
    1 "A"
    11 "J"
    12 "Q"
    13 "K"
    (str rank)))

(def ^:private suits [:clubs :diamonds :hearts :spades])

(def ^:private suit-symbol
  {:clubs    "\u2663"
   :diamonds "\u2666"
   :hearts   "\u2665"
   :spades   "\u2660"})

(def ^:private suit-color
  {:clubs    "black"
   :diamonds "crimson"
   :hearts   "crimson"
   :spades   "black"})

(defrecord Card [suit rank]
  Object
  (toString [_] (str (rank-symbol rank) (suit-symbol suit))))

;;;;

(def ^:private state
  (sgn/source {:phase :select
               :deck  (for [suit suits
                            rank (range 1 14)]
                        (->Card suit rank))}))

;;;;

(defn- card-view [i card]
  (el :div :class "card"
      :style {:color     (smap (comp suit-color :suit) card)
              :transform (str "translate(" (* 35 i) "px)")}
      (smap str card)))

(defn- ui-main [state]
  (let [deck (smap :deck state)]
    (el :div
        (->> deck
             (smap (partial map-indexed (fn [i _] [i (smap #(nth % i nil) deck)])))
             (smap (map-index-cached (fn [[i card]] (card-view i card))))))))

;;;;

(defn- main []
  (let [parent (.getElementById js/document "app-root")]
    (append-child! parent (ui-main state))))
