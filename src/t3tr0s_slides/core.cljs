(ns t3tr0s-slides.core
  (:require
    [t3tr0s-slides.slide01 :as slide01]
    [t3tr0s-slides.slide02 :as slide02]
    [t3tr0s-slides.slide03 :as slide03]
    [t3tr0s-slides.slide04 :as slide04]
    [t3tr0s-slides.slide05 :as slide05]
    [t3tr0s-slides.slide06 :as slide06]
    [t3tr0s-slides.slide07 :as slide07]
    [t3tr0s-slides.slide08 :as slide08]
    [t3tr0s-slides.slide09 :as slide09]
    [t3tr0s-slides.slide10 :as slide10]
    [t3tr0s-slides.slide11 :as slide11]
    ))

(enable-console-print!)

(.addEventListener js/window "load" slide11/init)
