(ns t3tr0s-slides.core
  (:require
    [t3tr0s-slides.slide1 :as slide1]
    [t3tr0s-slides.slide2 :as slide2]
    [t3tr0s-slides.slide3 :as slide3]
    [t3tr0s-slides.slide4 :as slide4]
    [t3tr0s-slides.slide5 :as slide5]
    [t3tr0s-slides.slide6 :as slide6]
    [t3tr0s-slides.slide7 :as slide7]
    [t3tr0s-slides.slide8 :as slide8]
    [t3tr0s-slides.slide9 :as slide9]
    ))

(enable-console-print!)

(.addEventListener js/window "load" slide9/init)
