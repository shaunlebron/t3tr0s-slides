(ns t3tr0s-slides.core
  (:require
    [t3tr0s-slides.slide1 :as slide1]))

(enable-console-print!)

(.addEventListener js/window "load" slide1/init)
