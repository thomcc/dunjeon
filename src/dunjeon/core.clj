(ns dunjeon.core
  (:import [java.awt Color Graphics2D Dimension Font]
           [javax.swing JFrame JPanel]
           [java.awt.event KeyEvent KeyAdapter]))

(def game-width 50)
(def game-height 50)

(defn random
  ([n] (random 0 n))
  ([min range] (+ min (rand-int range))))

(defn rand-elt [set] (rand-nth (vec set)))

(defn signum [x] (if (zero? x) x (if (pos? x) 1 -1)))

(def panel-width (* 11 game-width))

(def panel-height (* 11 (+ game-height 5)))

(def char-rep {:floor ".", nil "#", :wall "#", :stairs ">", :gold "$", :booze "q",
               :sword "(", :armor "[", :shield "+", :spawner "!", :player "@"})

(def color-rep {:floor Color/white, nil Color/gray, :wall Color/gray, :stairs Color/white
                :gold Color/yellow :booze Color/pink :sword Color/blue :shield Color/blue
                :armor Color/blue :spawner Color/red, :player Color/green})

(def directions {:north [0 -1], :south [0 1], :east [1 0], :west [-1 0]})

(def distribution {:gold 5, :booze (random 2 3), :sword (random 2 3),
                   :armor (rand-int 5), :shield (rand-int 2) :stairs 1})

(defn intersects [{x0 :x, y0 :y, w0 :w, h0 :h}, {x1 :x, y1 :y, w1 :w, h1 :h}]
  (and (or (and (>= x0 x1) (<= x0 (+ x1 w1))) (and (>= x1 x0) (<= x1 (+ x0 w0))))
       (or (and (>= y0 y1) (<= y0 (+ y1 h1))) (and (>= y1 y0) (<= y1 (+ y0 h0))))))

(defn room [{:keys [width height]}]
  {:x (random 2 (- width 13)) :y (random 2 (- width 13)) :w (random 3 10) :h (random 3 10)})

(defn add-room [{rooms :rooms :as level}]
  (letfn [(available? [r] (not-any? #(intersects r %) rooms))]
    (loop [r (room level), i 0]
      (cond (available? r) (assoc level :rooms (conj rooms r))
            (> i 1000) level
            :else (recur (room level) (inc i))))))

(defn empty-level [w h] {:width w, :height h, :rooms #{}})

(defn connect [{:keys [width height]}
               {fx :x, fy :y, fw :w, fh :h}
               {tx :x, ty :y, tw :w, th :h}]
  (let [x0 (random fx fw), y0 (random fy fh)
        x1 (random tx tw), y1 (random ty th)
        dx (signum (- x1 x0)), dy (signum (- y1 y0))]
    (loop [x x0, y y0, points #{}, horizontal? (not= (rand-int 2))]
      (if (and (= x x1) (= y y1)) points
          (let [[x y] (if (or (= y y1) (and horizontal? (not= x x1))) [(+ x dx) y]
                          [x (+ y dy)])]
            (if-not (and (> x 0) (> y 0) (< x width) (< y height)) points
                    (recur x y (conj points [x y]) (not= 0 (rand-int 10)))))))))

(defn connect-rooms [{:keys [width, height, rooms] :as level}]
  (loop [from (rand-elt rooms), conn #{from}, unconn (disj rooms from), paths #{}]
    (if (empty? unconn) (assoc level :paths paths)
      (let [to (rand-elt unconn), unconn (disj unconn to), conn (conj conn to)]
        (recur (rand-elt conn), conn, unconn, (conj paths (connect level from to)))))))

(defn pointify [{x :x, y :y, width :w, height :h}]
  (mapcat (fn [[row y]] (map #(vector % y) row))
          (partition 2 (interleave (repeat height (range x (+ x width)))
                                   (range y (+ y height))))))

(defn pointify-map [{:keys [width, height, rooms, paths]}]
  {:width width, :height height
   :points (merge (zipmap (mapcat pointify rooms) (repeat :floor))
                  (zipmap (apply concat paths) (repeat :floor)))})

(defn update-tile [level pos tile] (assoc-in level [:points pos] tile))

(defn place-randomly
  ([level tile] (place-randomly level 1 tile))
  ([level n tile]
     (reduce (fn [{p :points :as l} t] (update-tile l ((rand-elt p) 0) t))
             level
             (repeat n tile))))

(defn add-monsters [{:keys [points] :as level} n]
  (let [p (take n (shuffle (vec points)))]
    (assoc (reduce #(update-tile %1 (%2 0) :spawner) level p) :spawners p)))

(defn finalize [level]
  (-> (reduce (fn [lvl [item, num]] (place-randomly lvl num item)) level distribution)
      (add-monsters (rand-int 10))))

(defn gen-level [width height rooms]
  (-> (nth (iterate add-room (empty-level width height)) rooms)
      connect-rooms pointify-map finalize))

(defn draw-level [{width :width, height :height lvl :points}]
  (doseq [y (range height), x (range width)]
    (when (= 0 x) (.println System/out))
    (.print System/out (char-rep (lvl [x y])))))

(defn initialize-gamestate [level]
  {:level level, :player {:pos ((rand-elt (:points level)) 0),
                          :health 50 :inv #{}, :score 0}})

(defn move [pos dir] (map + pos (directions dir)))
(defmulti tick-player (fn [gamestate [kind & args]] kind))
(defmethod tick-player :default [game-state _] game-state)
(defmethod tick-player :move [{{pos :pos} :player, level :level :as gs} [_ dir]]
  (let [newpos (move pos dir)]
    (if ((:points level) newpos) (assoc-in gs [:player :pos] newpos) gs)))

(defmethod tick-player :action [{{:keys [pos inv]} :player, {pts :points} :level :as gs} [_ dir]]
  (let [thing (pts pos)]
    (condp #(% %2) thing
      #{:sword :armor :shield} (-> gs (update-in [:player :inv] conj thing)
                                   (assoc-in [:level :points pos] :floor))
      #{:booze} (update-in gs [:player :health] (partial + (rand-int 10)))
      #{:gold} (-> gs (update-in [:player :score] (partial + 100))
                   (assoc-in [:level :points pos] :floor))
      #{:spawner} (assoc-in gs [:level :points pos] :floor)
      gs)))

(defn tick [game-state input] (-> game-state (tick-player input)))

(defn comprehend [input]
  ({KeyEvent/VK_UP [:move :north], KeyEvent/VK_DOWN [:move :south]
    KeyEvent/VK_RIGHT [:move :east], KeyEvent/VK_LEFT [:move :west]
    KeyEvent/VK_ENTER [:action]}
   (.getKeyCode input)))

(defn draw [^Graphics2D g {{pos :pos h :health s :score} :player,
                           {:keys [points width height]} :level}]
  (doto g
    (.setColor Color/black)
    (.fillRect 0 0 panel-width panel-height)
    (.setFont (Font. "Monospaced" Font/PLAIN 14)))
  (doseq [xx (range width), yy (range height)]
    (let [type (if (= [xx yy] pos) :player (points [xx yy]))]
      (.setColor g (color-rep type))
      (.drawString g (char-rep type) (* xx 11) (* yy 11))))
  (doto g
    (.setColor Color/white)
    (.drawString (str "Health: " h) 11 (* 11 (+ game-height 1)))
    (.drawString (str "Score: " s) 11 (* 11 (+ game-height 2)))))

(defn -main [& args]
  (let [game-state (atom (initialize-gamestate (gen-level 50 50 (random 4 5))))
        dim (Dimension. panel-width panel-height)
        panel (doto (proxy [JPanel] [] (paint [g] (draw g @game-state)))
                (.setMinimumSize dim)
                (.setPreferredSize dim)
                (.setMaximumSize dim))
        ka (proxy [KeyAdapter] []
             (keyPressed [e]
               (when-let [input (comprehend e)]
                 (swap! game-state tick input)
                 (.repaint panel))))]
    (doto (JFrame. "(the dunjeon)")
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.add panel) .pack
      (.setResizable false)
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.addKeyListener ka))))


