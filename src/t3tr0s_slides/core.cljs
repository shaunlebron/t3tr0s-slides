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
    [t3tr0s-slides.slide12 :as slide12]
    [t3tr0s-slides.slide13 :as slide13]
    ))

(enable-console-print!)

(def slides
  [{:id "slide01" :init slide01/init :resume slide01/resume :stop slide01/stop}
   {:id "slide02" :init slide02/init :resume slide02/resume :stop slide02/stop}
   {:id "slide03" :init slide03/init :resume slide03/resume :stop slide03/stop}
   {:id "slide04" :init slide04/init :resume slide04/resume :stop slide04/stop}
   {:id "slide05" :init slide05/init :resume slide05/resume :stop slide05/stop}
   {:id "slide06" :init slide06/init :resume slide06/resume :stop slide06/stop}
   {:id "slide07" :init slide07/init :resume slide07/resume :stop slide07/stop}
   {:id "slide08" :init slide08/init :resume slide08/resume :stop slide08/stop}
   {:id "slide09" :init slide09/init :resume slide09/resume :stop slide09/stop}
   {:id "slide10" :init slide10/init :resume slide10/resume :stop slide10/stop}
   {:id "slide11" :init slide11/init :resume slide11/resume :stop slide11/stop}
   {:id "slide12" :init slide12/init :resume slide12/resume :stop slide12/stop}
   {:id "slide13" :init slide13/init :resume slide13/resume :stop slide13/stop}])

(def current-slide (atom nil))

(defn on-slide-change
  [_ _ i-prev i]
  (doseq [[j slide] (map-indexed vector slides)]
    (let [pos (-> (- j i) (* 100) (+ 50) (str "%"))
          elm (.getElementById js/document (:id slide))]
      (if (nil? i-prev)
        (.css      (js/$ elm) #js {:left pos})
        (.velocity (js/$ elm) #js {:left pos})))))

(add-watch current-slide :slide on-slide-change)

(defn- on-hash-change []
  (let [hash- (.replace (aget js/document "location" "hash") #"^#" "")]
    (when (= hash- "")
      (aset js/document "location" "hash" "0"))
    (js/console.log (pr-str hash-))
    (let [slide-number (js/parseInt hash-)
          slide (get slides slide-number)]
      (when slide
        (reset! current-slide slide-number)))))

(defn init-slides!
  []
  (doseq [{:keys [id init]} slides]
    (println id)
    (init id)))

(defn init []
  (init-slides!)

  (aset js/window "onhashchange" on-hash-change)
  (on-hash-change)
  )

(.addEventListener js/window "load" init)
