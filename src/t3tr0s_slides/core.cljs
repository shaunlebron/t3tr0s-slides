(ns t3tr0s-slides.core
  (:require
    [t3tr0s-slides.slide1 :as slide1]
    [t3tr0s-slides.slide2 :as slide2]
    [t3tr0s-slides.slide3 :as slide3]
    [t3tr0s-slides.slide4 :as slide4]
    ))

(enable-console-print!)

(.addEventListener js/window "load" slide4/init)
