(ns t3tr0s-slides.core
  (:require
    [t3tr0s-slides.slide1 :as slide1]
    [t3tr0s-slides.slide2 :as slide2]
    ))

(enable-console-print!)

(.addEventListener js/window "load" slide2/init)
