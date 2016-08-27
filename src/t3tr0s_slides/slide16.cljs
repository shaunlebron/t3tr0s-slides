(ns t3tr0s-slides.slide16
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cljs.core.async :refer [put! take! close! <! >! alts! chan timeout]]
    [rum.core :as rum]
    [t3tr0s-slides.state :refer [current-slide]]
    [t3tr0s-slides.syntax-highlight :as sx]))

(def dark-green "#143")
(def light-green "#175")
(def dark-purple "#449")
(def light-purple "#6ad")
(def dark-red "#944")
(def light-red "#f8c")

(def pieces
  {:I [[-1  0] [ 0  0] [ 1  0] [ 2  0]]
   :L [[ 1 -1] [-1  0] [ 0  0] [ 1  0]]
   :J [[-1 -1] [-1  0] [ 0  0] [ 1  0]]
   :S [[ 0 -1] [ 1 -1] [-1  0] [ 0  0]]
   :Z [[-1 -1] [ 0 -1] [ 0  0] [ 1  0]]
   :O [[ 0 -1] [ 1 -1] [ 0  0] [ 1  0]]
   :T [[ 0 -1] [-1  0] [ 0  0] [ 1  0]]})

(defn rotate-coord [[x y]] [(- y) x])
(defn rotate-piece [piece] (mapv rotate-coord piece))

(def nrows 20)
(def ncols 10)
(def empty-row (vec (repeat ncols 0)))
(def empty-board (vec (repeat nrows empty-row)))
(def filled-board
  [[ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]])

(def initial-pos [5 2])

(def app (atom {:board filled-board
                :piece (rand-nth (vals pieces))
                :position initial-pos}))

(defn write-piece
  [board coords [cx cy] value]
  (if-let [[x y] (first coords)]
    (recur (try (assoc-in board [(+ y cy) (+ x cx)] value)
                (catch js/Error _ board))
           (rest coords)
           [cx cy]
           value)
    board))

(declare get-drop-pos)

(defn create-drawable-board
  [board piece [x y]]
  (let [gy    (get-drop-pos board  piece [x y])
        board1 (write-piece board  piece [x gy] "G")
        board2 (write-piece board1 piece [x y ] "P")]
    board2))

(defn app-drawable-board!
  []
  (let [board (:board @app)
        piece (:piece @app)
        pos (:position @app)]
    (create-drawable-board board piece pos)))

(defn lock-piece! []
  (let [{:keys [piece position]} @app]
    (swap! app
      update-in [:board]
        write-piece piece position 1)))

(defn piece-fits?
  [board piece [cx cy]]
  (every? (fn [[x y]]
            (zero? (get-in board [(+ y cy) (+ x cx)])))
          piece))

(defn app-piece-fits?
  []
  (boolean (piece-fits? (:board @app) (:piece @app) (:position @app))))

(defn try-shift! [dx]
  (when-let [piece (:piece @app)]
    (let [[x y] (:position @app)
          board (:board @app)
          new-pos [(+ x dx) y]]
      (when (piece-fits? board piece new-pos)
        (swap! app assoc :position new-pos)))))

(defn try-rotate! []
  (when-let [piece (:piece @app)]
    (let [pos (:position @app)
          board (:board @app)
          new-piece (rotate-piece piece)]
      (when (piece-fits? board new-piece pos)
        (swap! app assoc :piece new-piece)))))

(defn get-drop-pos
  [board piece [x y]]
  (let [collide? (fn [cy] (not (piece-fits? board piece [x cy])))
        cy (first (filter collide? (iterate inc y)))]
    (max y (dec cy))))

(defn filled-rows
  [board]
  (->> (map-indexed vector board)
       (filter (fn [[i row]] (every? pos? row)))
       (map first)
       (apply hash-set)))

(defn collapse-rows
  [board rows]
  (let [cleared-board (->> board
                           (map-indexed vector)
                           (remove #(rows (first %)))
                           (map second))
        n (count rows)
        new-board (into (vec (repeat n empty-row)) cleared-board)]
    new-board))

(defn clear-rows
  [board rows]
  (vec (map-indexed
        (fn [i row]
          (if (rows i) empty-row row)) board)))

(def resume-state (atom nil))
(def prev-filled (atom nil))
(def prev-cleared (atom nil))
(def prev-collapsed (atom nil))
(def anim-index (atom nil))

(defn on-change-anim-index
  [_ _ _ i]
  (when i
    (reset! app (get [@prev-cleared
                      @prev-filled
                      @prev-cleared
                      @prev-collapsed] i))))

(add-watch anim-index :history on-change-anim-index)

(defn animate-collapse! []
  (let [board     (:board @app)
        rows      (filled-rows board)
        cleared   (clear-rows board rows)
        collapsed (collapse-rows board rows)]

    (reset! prev-filled    (assoc @app :board board))
    (reset! prev-cleared   (assoc @app :board cleared))
    (reset! prev-collapsed (assoc @app :board collapsed))

    (when (seq rows)
      (go
        (dotimes [i 3]

          (reset! anim-index 0)
          (<! (timeout 170))

          (reset! anim-index 1)
          (<! (timeout 170)))

        (reset! anim-index 2)
        (<! (timeout 220))

        (reset! anim-index 3)
        (<! (timeout 170))

        (reset! anim-index nil)))))


(defn spawn-piece! []
  (swap! app assoc :position initial-pos
                   :piece (rand-nth (vals pieces))))

(defn clear-piece! []
  (swap! app assoc
         :piece nil
         :position nil))

(def halt-chan)
(defn stop-gravity! []
  (close! halt-chan))

(declare soft-drop!)
(defn start-gravity! []
  (when (= @current-slide 16)
    (set! halt-chan (chan))
    (go-loop []
      (let [[_ c] (alts! [(timeout 500) halt-chan])]
        (when (not= c halt-chan)
          (soft-drop!)
          (recur))))))

(defn piece-done! []
  (lock-piece!)
  (clear-piece!)
  (stop-gravity!)
  (go
    (when (seq (filled-rows (:board @app)))
      (<! (animate-collapse!)))
    (spawn-piece!)
    (start-gravity!)))

(defn soft-drop! []
  (let [{:keys [piece board position]} @app
        [x y] position
        new-pos [x (inc y)]]
    (if (piece-fits? board piece new-pos)
      (swap! app assoc :position new-pos)
      (piece-done!))))

(defn hard-drop! []
  (let [piece (:piece @app)
        [x y] (:position @app)
        board (:board @app)
        ny (get-drop-pos board piece [x y])]
    (swap! app assoc :position [x ny])
    (piece-done!)))

(defn data-row
  [index content]
  [:span
   {:class (if (= @anim-index index) "active-row-534ed" "")
    :onMouseEnter
    (fn []
      (when (nil? @anim-index)
        (reset! resume-state @app))
      (when @prev-filled
        (reset! anim-index index)))}
   content])

(rum/defc code []
  [:.code-cb62a
   [:pre
    [:code
      "(" (sx/core "defn") " start-gravity! []\n"
      "  (" (sx/kw "go-loop") " []\n"
      "    (" (sx/kw "<!") " (" (sx/kw "timeout") " " (sx/lit "500") "))\n"
      "    (soft-drop!)\n"
      "    (" (sx/core "recur") ")))\n"]]])

(def cell-size (quot 600 nrows))

(defn draw-cell!
  [ctx [x y] is-center]
  (set! (.. ctx -lineWidth) 2)
  (let [rx (* cell-size x)
        ry (* cell-size y)
        rs cell-size
        cx (* cell-size (+ x 0.5))
        cy (* cell-size (+ y 0.5))
        cr (/ cell-size 4)]

    (.. ctx (fillRect rx ry rs rs))
    (.. ctx (strokeRect rx ry rs rs))
    (when is-center
      (.. ctx beginPath)
      (.. ctx (arc cx cy cr 0 (* 2 (.-PI js/Math))))
      (.. ctx fill)
      (.. ctx stroke))))



(defn piece-abs-coords
  [piece [cx cy]]
  (mapv (fn [[x y]] [(+ cx x) (+ cy y)]) piece))

(defn draw-piece!
  [ctx piece pos]
  (doseq [[i c] (map-indexed vector (piece-abs-coords piece pos))]
    (draw-cell! ctx c (= c pos))))

(defn draw-board!
  [ctx board]
  (let [filled? (filled-rows board)]
    (doseq [y (range nrows)
            x (range ncols)]
      (set! (.. ctx -globalAlpha) (if (filled? y) 0.3 1))
      (let [v (get-in board [y x])]
        (when-not (zero? v)
          (draw-cell! ctx [x y] false))))))


(defn draw-canvas!
  [canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size ncols) (* cell-size nrows)))

    (set! (.. ctx -fillStyle)   dark-green)
    (set! (.. ctx -strokeStyle) light-green)
    (draw-board! ctx (:board @app))
    (set! (.. ctx -globalAlpha) 1)

    (when-let [piece (:piece @app)]
      (let [pos (:position @app)
            drop-y (get-drop-pos (:board @app) piece pos)
            drop-pos (assoc pos 1 drop-y)
            fits (app-piece-fits?)]

        (set! (.. ctx -fillStyle) "#555")
        (set! (.. ctx -strokeStyle) "#AAA")
        (let [places (- (second pos) (dec (second initial-pos)))
              [x y] pos
              q 5]
          (dotimes [i places]
            (set! (.. ctx -globalAlpha) (/ (max 0 (min q (- i (- places q)))) q 5))
            (draw-piece! ctx piece [x (- y (- places i))])))

        (set! (.. ctx -globalAlpha) 1)
        (when (and piece pos)
          ;(set! (.. ctx -fillStyle)   "#333")
          ;(set! (.. ctx -strokeStyle) "#666")
          ;(draw-piece! ctx piece drop-pos)
          (set! (.. ctx -fillStyle)   (if fits dark-purple dark-red))
          (set! (.. ctx -strokeStyle) (if fits light-purple light-red))
          (draw-piece! ctx piece pos))))))

(def key-names
  {37 :left
   38 :up
   39 :right
   40 :down
   32 :space
   82 :r})

(def key-name #(-> % .-keyCode key-names))

(defn resume-control!
  []
  (when (and (not (nil? @anim-index))
             (not (nil? @resume-state)))
    (reset! app @resume-state)
    (reset! resume-state nil)
    (reset! anim-index nil)))

(defn key-down [e]
  (let [kname (key-name e)
        piece (:piece @app)]
   (case kname
     :left  (when piece (try-shift! -1))
     :right (when piece (try-shift! 1))
     :up    (when piece (try-rotate!))
     :down  (when piece (soft-drop!))
     :space (when piece (hard-drop!))
     :r     (resume-control!)
     nil)
   (when (#{:down :left :right :space :up} kname)
     (.preventDefault e))))

(def canvas-mixin
  {:did-mount
   (fn [state]
     (let [canvas (rum/ref state "canvas")]
      (set! (.. canvas -width) (* ncols cell-size))
      (set! (.. canvas -height) (* nrows cell-size))
      (draw-canvas! canvas)
      state))
   :did-update
   (fn [state]
     (let [canvas (rum/ref state "canvas")]
      (draw-canvas! canvas)
      state))})

(rum/defc canvas < canvas-mixin []
  [:.canvas-2a4d7
   [:canvas
    {:ref "canvas"
     :style {:position "relative"}}]])

(rum/defc slide []
  [:div
   [:h1 "16. Add gravity."]
   (code)
   (canvas)])

(def slide-elm)
(defn render []
  (rum/mount (slide) slide-elm))

(defn init [id]
  (set! slide-elm (js/document.getElementById id))
  (render)
  (add-watch app :render render))

(defn resume []
  (start-gravity!)
  (.addEventListener js/window "keydown" key-down))

(defn stop []
  (stop-gravity!)
  (.removeEventListener js/window "keydown" key-down))
