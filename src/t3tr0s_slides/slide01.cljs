(ns t3tr0s-slides.slide01
  (:require
    [om.core :as om :include-macros true]
    [om-tools.core :refer-macros [defcomponent]]
    [sablono.core :refer-macros [html]]
    [t3tr0s-slides.syntax-highlight :as sx]
    ))

(def app-state (atom {:row nil :col nil}))

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))

(defn data-row
  [row app]
  [:span
   {:key (str "row" row)
    :class (if (= row (:row app)) "active-row-534ed" "")
    :onMouseEnter #(om/update! app :row row)
    :onMouseLeave #(om/update! app :row nil)
    }
    "["
    (for [col (range cols)]
      [:span 
       {:key (str "row" row "col" col)
        :class (if (and (= row (:row app))
                        (= col (:col app))) "active-col-d9099")
        :onMouseEnter #(om/update! app :col col)
        :onMouseLeave #(om/update! app :col nil)
        }
       (list " " (sx/lit (get-in empty-board [row col])))])
    " ]"])

(defcomponent code
  [app owner]
  (did-mount [_]
    (let [block (om/get-node owner "code")]
      (.highlightBlock js/hljs block))
    )
  (render
    [_]
    (html
      [:div.code-cb62a
       [:pre
        [:code#lang-clj
         {:ref "code"}
         "(" (sx/core "def") " rows " (sx/lit "20") ")\n"
         "(" (sx/core "def") " cols " (sx/lit "10") ")\n"
         "(" (sx/core "def") " empty-row (" (sx/core "vec") " (" (sx/core "repeat") " cols " (sx/lit "0") ")))\n"
         "(" (sx/core "def") " empty-board (" (sx/core "vec") " (" (sx/core "repeat") " rows empty-row)))\n"]
        [:code
         "\n"
         "empty-board\n\n"
         (for [row (range rows)]
           (condp = row
             0          (list "  [" (data-row row app) "\n")
             (dec rows) (list "   " (data-row row app) "]\n")
             (list "   " (data-row row app) "\n")))]
         
         ]])))

(def cell-size (quot 600 rows))

(defn canvas-mouse
  [app owner e]
  (let [canvas (om/get-node owner)
        rect (.getBoundingClientRect canvas)
        x (- (.-clientX e) (.-left rect) 20)
        y (- (.-clientY e) (.-top rect) 20)
        col (quot x cell-size)
        row (quot y cell-size)]
    (om/update! app :row row)
    (om/update! app :col col)))

(defn draw-canvas!
  [app canvas]
  (let [ctx (.. canvas (getContext "2d"))
        x (:col app)
        y (:row app)]
    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))
    (when (and x y)
      (set! (.. ctx -fillStyle) "#143")
      (set! (.. ctx -strokeStyle) "#175")
      (set! (.. ctx -lineWidth) 2)
      (let [rx (* cell-size x)
            ry (* cell-size y)
            rs cell-size]
        (.. ctx (fillRect rx ry rs rs))
        (.. ctx (strokeRect rx ry rs rs)))
      )))

(defcomponent canvas
  [app owner]
  (did-mount [_]
    (let [canvas (om/get-node owner "canvas")]
      (set! (.. canvas -width) (* cols cell-size))
      (set! (.. canvas -height) (* rows cell-size))
      (draw-canvas! app (om/get-node owner "canvas"))
      ))
  (did-update [_ _ _]
    (draw-canvas! app (om/get-node owner "canvas"))
    )
  (render [_]
    (html
      [:div.canvas-2a4d7
       [:canvas
        {:ref "canvas"
         :style {:position "relative"}
         :onMouseMove #(canvas-mouse app owner %)
         :onMouseLeave #(do (om/update! app :row nil)
                            (om/update! app :col nil))
         }
        ]])))

(defcomponent slide
  [app owner]
  (render
    [_]
    (html
      [:div
       (om/build code app)
       (om/build canvas app)])))

(defn init
  []
  (om/root
    slide
    app-state
    {:target (. js/document (getElementById "app"))}))
