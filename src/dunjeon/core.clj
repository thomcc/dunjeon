(ns dunjeon.core
  (:import [java.awt Color Graphics2D Dimension Font]
           [javax.swing JFrame JPanel]
           [java.awt.event KeyEvent KeyAdapter]))
(set! *warn-on-reflection* true)
;; constants and utility functions
(def game-width (int 50))
(def game-height (int 50))

(defn random
  ([n] (random 0 n))
  ([min range] (+ min (rand-int range))))

(defn rand-elt [set] (rand-nth (vec set)))

(defn signum [x] (if (zero? x) x (if (pos? x) 1 -1)))

(def panel-width (* 11 game-width))

(def panel-height (* 11 (+ game-height 5)))

(def char-rep {::floor ".", nil "#", :wall "#", ::stairs ">", ::gold "$", ::booze "!",
               ::sword "(", ::armor "[", ::shield "+",, ::player "@", ::monster "m"})

(def color-rep {::floor Color/white, nil Color/gray, ::wall Color/gray, ::stairs Color/white
                ::gold Color/yellow ::booze Color/pink ::sword Color/blue ::shield Color/blue
                ::armor Color/blue ::monster Color/red, ::player Color/green})

(def directions {:north [0 -1], :south [0 1], :east [1 0], :west [-1 0]})

(def distribution {::gold 5, ::booze (random 2 3), ::sword (random 2 3),
                   ::armor (rand-int 5), ::shield (rand-int 2) ::stairs 1})

;; random dungeon generation

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
            (if-not (and (> x 0) (> y 0) (< x width) (< y height)) points        ; i use not= instead of zero? here
                    (recur x y (conj points [x y]) (not= 0 (rand-int 10))))))))) ; because otherwise compiler weirdly complains.

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
  {:width width, :height height, :seen #{},
   :points (merge (zipmap (mapcat pointify rooms) (repeat ::floor))
                  (zipmap (apply concat paths) (repeat ::floor)))})

(defn update-tile [level pos tile] (assoc-in level [:points pos] tile))

(defn place-randomly [level n tile]
  (reduce (fn [{p :points :as l} t] (update-tile l ((rand-elt p) 0) t))
          level (repeat n tile)))

(defn make-monster [pos] {:pos pos, :health 10})

(defn add-monsters [{:keys [points] :as level} n]
  (let [mpts (map #(% 0) (take n (shuffle (vec points))))]
    (assoc level :monsters (set (map make-monster mpts)))))

(defn finalize [level]
  (-> (reduce (fn [lvl [item, num]] (place-randomly lvl num item)) level distribution)
      (add-monsters (rand-int 10))))

(defn gen-level [width height rooms]
  (-> (nth (iterate add-room (empty-level width height)) rooms)
      connect-rooms pointify-map finalize))

;; game logic

(defn initialize-gamestate [level]
  {:level level, :player {:pos ((rand-elt (:points level)) 0), :health 50 :inv [], :sees #{} :score 0} :messages ()})

(defn move [pos dir] (map + pos (directions dir)))
(defn clear-tile [gs pos] (assoc-in gs [:level :points pos] ::floor))
(defn add-msg [gs msg] (update-in gs [:messages] conj msg))

(derive ::sword  ::item)
(derive ::shield ::item)
(derive ::armor  ::item)
(defmulti use-tile (fn [gs pos item] item))
(defmethod use-tile :default [gs _ _] gs)
(defmethod use-tile ::floor [gs _ _] gs)

(defmethod use-tile ::item [game-state pos it]
  (-> game-state
      (clear-tile pos)
      (update-in [:player :inv] conj it)
      (add-msg (str "You pick up a shiny new " (name it) "."))))

(defmethod use-tile ::booze [game-state pos _]
  (let [healed (random 2 7)]
   (-> game-state
       (clear-tile pos)
       (update-in [:player :health] + healed)
       (add-msg (str "The booze heals you for " healed " points.")))))

(defmethod use-tile ::gold [game-state pos _]
  (-> game-state
      (clear-tile pos)
      (update-in [:player :score] + 25)
      (add-msg (str "You find 25 gold on the ground. Score!"))))

(defmethod use-tile ::stairs [{p :player :as game-state} _ _]
  (let [next-floor (gen-level 50 50 (random 4 5))]
    (-> game-state
        (assoc :level next-floor)
        (assoc-in [:player :pos] ((rand-elt (:points next-floor)) 0))
        (add-msg "You go down the stairs."))))

(defn fight [{{ms :monsters} :level :as gs} {h :health :as mon}]
  (let [d (random 3 5), left (- h d), damaged (assoc mon :health left), alive? (pos? left), ms (disj ms mon)]
    (-> gs
        (assoc-in [:level :monsters] (if alive? (conj ms damaged) ms))
        (add-msg (str "The monster " (if alive? (str "takes " d " damage.") "dies.")))
        (update-in [:player :score] + (if alive? 0 (random 25 25))))))

(defn monster-at [{{mons :monsters} :level} pos]
  (loop [[{p :pos :as m} & ms] (seq mons)]
    (cond (= p pos) m, (not ms) nil, :else (recur ms))))

(defn can-see? [{pts :points} [x y :as source] [fx fy :as end]]
  (or (and (= x fx) (= y fy))
      (let [[x2 y2 :as dest] (map #(+ %2 (/ (signum (- % %2)) 2.0)) source end)
            [distx disty :as dist] (map - dest source)
            length (max (Math/abs (double distx)) (Math/abs (double disty)))
            [dx dy :as delta] (map #(/ % length) dist)]
        (loop [len length, [xx yy :as pos] source]
          (or (neg? len)
              (let [ix (int (+ xx 0.5)), iy (int (+ yy 0.5))]
                (cond (and (= ix x2) (= iy y2)) true
                      (and (not (and (= ix x) (= iy y))) (not (pts [ix iy]))) false
                      :else (recur (dec len) [(+ dx xx) (+ dy yy)]))))))))

(defmulti tick-player (fn [gamestate [kind & args]] kind))

(defmethod tick-player :default [game-state _] game-state)

(defmethod tick-player :move [{{pos :pos} :player, level :level :as gs} [_ dir]]
  (let [newpos (move pos dir)]
    (if ((:points level) newpos)
      (if-let [m (monster-at gs newpos)] (fight gs m)
              (assoc-in gs [:player :pos] newpos))
      gs)))

(defmethod tick-player :action [{{p :pos} :player, {pts :points} :level :as gs} _]
  (use-tile gs p (pts p)))

(defn tick-monsters [{{ms :monsters, pts :points} :level {pp :pos} :player :as gs}]
  (assoc-in gs [:level :monsters]
            (set (map (fn [{p :pos :as m}]
                        (let [d (rand-nth (vals directions)),
                              npos (map + p d),
                              can? (and (pts npos) (not= pp npos))]
                          (cond can? (assoc m :pos npos)
                                :else m)))
                      ms))))


(defn update-vision [{{seen :seen :as lvl} :level,{[x y] :pos :as player} :player :as game-state}]
  (let [visible (for [xx (range -10 10), yy (range -10 10)
                      :when (and (<= (+ (* xx xx) (* yy yy)) 100)
                                 (can-see? lvl [x y] [(+ x xx) (+ y yy)]))]
                  [(+ xx x) (+ yy y)])]
    (-> game-state
        (assoc-in [:level :seen] (into seen visible))
        (assoc-in [:player :sees] (set visible)))))

(defn tick [game-state input]
  (-> game-state
      (tick-player input)
      tick-monsters
      update-vision))

(defn comprehend [^KeyEvent input]
  ({KeyEvent/VK_UP [:move :north], KeyEvent/VK_DOWN [:move :south],
    KeyEvent/VK_RIGHT [:move :east], KeyEvent/VK_LEFT [:move :west]
    KeyEvent/VK_ENTER [:action]}
   (.getKeyCode input)))

(defn draw [^Graphics2D g {{[px py :as pos] :pos h :health s :score sees :sees :as play} :player,
                           {:keys [points width height monsters seen]} :level
                           msgs :messages}]
  (doto g
    (.setColor Color/black)
    (.fillRect 0 0 panel-width panel-height)
    (.setFont (Font. "Monospaced" Font/PLAIN 13)))
  (let [monpts (set (map :pos monsters))
        post-process (fn [col sees? seen?]
                       (cond (not (or sees? seen?)) Color/black
                             (not sees?) (.darker (.darker ^Color col))
                             :else col))]
;    (prn monsters)
    (doseq [xx (range width), yy (range (inc height))]
;      (prn {:sees sees :seen seen :xx xx :yy yy :player play})
      (let [p [xx yy],
            type (cond (= p pos) ::player, (monpts p) ::monster, :else (points p))
            p-vis (sees p), p-rem (seen p)]
        (.setColor g (post-process (color-rep type) p-vis p-rem))
        (.drawString g ^String (char-rep type) (int (* xx 11)) (int (* yy 11))))))
  (.setColor g Color/white)
  (dorun 3 (map-indexed #(.drawString g ^String %2 (* 15 11)
                                      (int (* 11 (+ 4 game-height (- %))))) msgs))
  (doto g
    (.drawString (str "Health: " h) 11 (int (* 11 (inc game-height))))
    (.drawString (str "Score: " s) 11 (int (* 11 (+ game-height 2))))))

(defn -main [& args]
  (let [game-state (atom (update-vision (initialize-gamestate (gen-level game-width game-height (random 4 5)))))
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
    (doto (JFrame. "(dunjeon)")
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.add panel) .pack
      (.setResizable false)
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.addKeyListener ka))))


