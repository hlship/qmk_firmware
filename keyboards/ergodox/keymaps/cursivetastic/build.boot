(require 
    '[clojure.string :as str]
    '[clojure.java.io :as io])

(def keymap-arg-count 76)

(def keymap-pos
    "Maps keymap layer keyword to its position in KEYMAP"
    (zipmap (->> '[k50 k51 k52 k53 k54 k55 k56
     k40 k41 k42 k43 k44 k45 k46
     k30 k31 k32 k33 k34 k35
     k20 k21 k22 k23 k24 k25 k26
     k10 k11 k12 k13 k14
     k05 k06 k04
     k03 k02 k01

     k57 k58 k59 k5a k5b k5c k5d
     k47 k48 k49 k4a k4b k4c k4d
     k38 k39 k3a k3b k3c k3d
     k27 k28 k29 k2a k2b k2c k2d
     k19 k1a k1b k1c k1d
     k07 k08
     k09 k0c
     k0b k0a]
     (map keyword))
    (iterate inc 0)))

(defprotocol Render 
    (render [x]))


(extend-protocol Render

    clojure.lang.Keyword
    (render [k]
        (str "KC_" (-> k name str/upper-case))
        )

    java.lang.String
    (render [s] s)

    java.lang.Number
    (render [n] (str n)))

(defrecord FnCall [fn-name wrapped]
    Render
    (render [_]
        (str fn-name "(" (render wrapped) ")")))

(def fn-call ->FnCall)

(defn keymap [ks]
    (let [ordered (reduce-kv 
        (fn [args k v]
            (assoc args (keymap-pos k) (render v)))
        (vec (repeat keymap-arg-count  "KC_TRNS"))
        ks)]
    (str "KEYMAP("
        (str/join ", " ordered)
        ")")))

(defn keymaps [keymaps]
    (dotimes [i (count keymaps)]
        (println (str "[" i "] = "
        (keymap (get keymaps i))
        ","))))

(deftask write-keymap 
  "Writes the keymap to a file."
  [f file PATH file "Output file."]
  (println "Writing to:" (.getPath file))

  (binding [*out* (io/writer file)]

      (println (keymaps {:k50 "RESET"
        :k51 :1
        :k52 :2
        :k0a :spc}))))

