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
        (str "KC_" (-> k 
            name
            str/upper-case
            (str/replace "-" "_"))))

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
    ;; KEYMAP is a macro from ez.h that converts its arguments into nested arrays
    ;; with an even more arcane ordering.
    (str "KEYMAP("
        (str/join ", " ordered)
        ")")))

(defn control [x] (fn-call "LCTL" x))
(defn alt [x] (fn-call "LALT" x))
(defn gui [x] (fn-call "LGUI" x))

(def mod-values
   {:control "MOD_LCTL"
   :shift "MOD_LSFT"
   :alt "MOD_LALT"
   :gui "MOD_LGUI"
   })

(defn momentary 
    "Change layer while key held"
    [layer] 
    (fn-call "MO" layer))

(defn one-shot 
    "Activate layer just for next key"
    [layer] (fn-call "OSL" layer))

(defn toggle 
    "Toggle layer until layer is toggled again"
    [layer] (fn-call "TG" layer))

(defn keymaps [& keymaps]
    (let [vk (vec keymaps)]
        (dotimes [i (count keymaps)]
            (println (str "[" i "] = "
                (keymap (get vk i))
                ",")))))

(deftask write-keymap 
  "Writes the keymap to a file."
  [f file PATH file "Output file."]
  (println "Writing to:" (.getPath file))

  (binding [*out* (io/writer file)]
    (keymaps {:k50 :esc
        :k51 :1
        :k52 :2  
        :k53 :3
        :k54 :4
        :k55 :5
        :k56 :lalt
        :k40 :tab
        :k41 :q
        :k42 :w
        :k43 :e
        :k44 :r
        :k45 :t
        :k46 :lctrl
        :k30 :caps
        :k31 :a
        :k32 :s
        :k33 :d
        :k34 :f
        :k35 :g
        :k20 :lshift
        :k21 :z
        :k22 :x
        :k23 :c
        :k24 :v
        :k25 :b
        :k26 :lgui
        :k11 :grave
        :k12 :bslash
        :k13 :left
        :k14 :right
        :k03 :bspace
        :k02 :delete
        :k05 :lbracket
        :k04 :home
        :k01 :end

        ; :k10 - Clojure key
        :k06  (momentary 1)

        :k57 :ralt
        :k58 :6
        :k59 :7
        :k5a :8
        :k5b :9
        :k5c :0
        :k5d :equal
        :k47 :rctrl 
        :k48 :y
        :k49 :u
        :k4a :i
        :k4b :o
        :k4c :p
        :k4d :minus
        :k38 :h
        :k39 :j
        :k3a :k
        :k3b :l
        :k3c :scolon
        :k3d :quote
        :k27 :rgui
        :k28 :n
        :k29 :m 
        :k2a :comma
        :k2b :dot
        :k2c :slash
        :k2d :rshift
        :k19 :left
        :k1a :down
        :k1b :up
        :k1c :right
        ; :k1d - Clojure key
        :k0a :space
        :k0b :enter
        :k08 :rbracket
        :k07 (momentary 1)
        :k09 :pgup
        :k0c :pgdown
    }
        ;; Layer 1: Function keys and A/V controls
        {
            :k51 :f1
            :k52 :f2  
            :k53 :f3
            :k54 :f4
            :k55 :f5
            :k58 :f6
            :k59 :f7
            :k5a :f8
            :k5b :f9
            :k5c :f10
            :k4c :f11
            :k3c :f12
            :k2c :f13
            :k1c :f14

            :k4a :up
            :k39 :left
            :k3a :down
            :k3b :right

            :k42 :ms-up
            :k31 :ms-left
            :k32 :ms-down
            :k33 :ms-right
            :k41 :ms-btn1
            :k43 :ms-btn2

            :k29 :audio-mute
            :k2a :audio-vol-down
            :k2b :audio-vol-up

            :k23 :media-play-pause
            :k24 :media-prev-track
            :k25 :media-next-track

            :k5d (toggle 2)


        }
;; Layer 2: Numeric Keypad
    {
        :k50 "RESET"  ; Easier than hitting the reset button

        :k49 :kp-7
        :k4a :kp-8
        :k4b :kp-9
        :k39 :kp-4
        :k3a :kp-5
        :k3b :kp-6
        :k29 :kp-1
        :k2a :kp-2
        :k2b :kp-3
        :k1b :kp-dot
        :k5a :kp-slash
        :k5b :kp-asterisk
        :k5c :kp-minus
        :k4c :kp-plus
        :k3c :kp-plus
        :k2c :kp-enter 
        :k1c :kp-enter
        :k0a :kp-0

    }


                )))