(ns t3tr0s-slides.syntax-highlight)

(defn core [& s] [:span.syntax-core-4c264 s])
(defn kw   [& s] [:span.syntax-kw-18cc2 s])
(defn lit  [& s] [:span.syntax-lit-60e83 s])
(defn cmt  [& s] [:span.syntax-cmt-d159d s])
(defn out  [& s] [:span.syntax-out-3c466 s])

(defn old [& s] [:span.syntax-old s])
(defn new [& s] [:span.syntax-new s])
