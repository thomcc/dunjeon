(ns dun.utils)

(defn random "wrapper for rand-int which takes an offset"
  ([n] (random 0 n))
  ([min range] (+ min (rand-int range))))

(defn rand-elt "choose a random element in a set"
  [set] (rand-nth (vec set)))

(defn signum "replacement for Math/signum which throws if x is 0"
  [x] (if (zero? x) x (if (pos? x) 1 -1)))

