(ns keymapping
  (:refer-clojure :exclude [key])
  (:require
    [clojure.string :as str]
    [clojure.set :refer [map-invert]])
  (:import
    (clojure.lang Keyword)))


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

(def key*
  "Map common QWERTY keys to keymap pos key. This is the majority of layer zero.

  The keys and values are converted to keywords.  This is not only used to define
  layer 0 (by inverting the map), but is also used by the key macro to convert
  key names back into keymap position keywords."
  (reduce-kv (fn [m k v]
               ;; Convert to string first, allows numbers to be converted to
               ;; keywords.
               (assoc m (-> k str keyword) (keyword v)))
             {}
             '{q        k41
               w        k42
               e        k43
               r        k44
               t        k45
               y        k48
               u        k49
               i        k4a
               o        k4b
               p        k4c
               a        k31
               s        k32
               d        k33
               f        k34
               g        k35
               h        k38
               j        k39
               k        k3a
               l        k3b
               scolon   k3c                                 ; semicolon
               z        k21
               x        k22
               c        k23
               v        k24
               b        k25
               n        k28
               m        k29
               comma    k2a
               dot      k2b
               slash    k2c
               1        k51
               2        k52
               3        k53
               4        k54
               5        k55
               6        k58
               7        k59
               8        k5a
               9        k5b
               0        k5c
               esc      k50
               tab      k40
               grave    k11                                 ; backtick
               bslash   k12
               delete   k02
               lbracket k05
               home     k04
               end      k01
               bspace   k03
               equal    k5d
               minus    k4d
               quote    k3d
               space    k0a
               enter    k0b
               rbracket k08
               pgup     k09
               pgdown   k0c
               left     k19                                 ; bottom row of RHS
               down     k1a
               up       k1b
               right    k1c
               }))

;; Extra left/right keys (under C and V on left hand keyboard)
(def lh-left :k13)
(def lh-right :k14)

(defmacro key [x]
  (let [k (-> x str keyword key*)]
    (or k
        (throw (ex-info "Bad param to key."
                        {:key        x
                         :valid-keys (keys key*)})))))

(defprotocol Render
  (render [x]))

(extend-protocol Render

  Keyword
  (render [k]
    (str "KC_" (-> k
                   name
                   str/upper-case
                   (str/replace "-" "_"))))

  String
  (render [s] s)

  Number
  (render [n] (str n)))

(defrecord FnCall [fn-name wrapped]
  Render
  (render [_]
    (str fn-name "(" (render wrapped) ")")))

(def fn-call ->FnCall)

(defn keymap [ks]
  (let [ordered (reduce-kv
                  (fn [args k v]
                    (let [p (keymap-pos k)]
                      (when (nil? p)
                        (throw (ex-info "Bad keymap key."
                                        {:key    k
                                         :value  v
                                         :keymap ks})))
                      (assoc args p (render v))))
                  (vec (repeat (count keymap-pos) "KC_TRNS"))
                  ks)]
    ;; KEYMAP is a macro from ez.h that converts its arguments into nested arrays
    ;; with an even more arcane ordering.
    (str "KEYMAP("
         (str/join ",\n  " ordered)
         ")")))

(defn ctrl [x] (fn-call "LCTL" x))
(defn alt [x] (fn-call "LALT" x))
(defn gui [x] (fn-call "LGUI" x))
(defn shift [x] (fn-call "LSFT" x))
(defn ctrl-alt-gui [x] (fn-call "LCAG" x))

(def ctrl-shift (comp shift ctrl))
(def ctrl-alt (comp ctrl alt))
(def ctrl-gui (comp ctrl gui))
(def alt-shift (comp alt shift))
(def alt-gui (comp alt gui))
(def gui-shift (comp gui shift))

(def macro-id-generator
  "Generator of unique ids for macros."
  (atom 0))

(defprotocol Macro

  (macro-id [this]
    "Unique macro id. Used for the case statement.")

  (macro-source [this]
    "Returns string of C source code for when event pressed for this macro."))

(defn macro
  "Defines a simple macro in terms of sequence of keys to be pressed.

  The terms must be renderable (e.g., keywords)."
  [& terms]
  (let [id (swap! macro-id-generator inc)]
    (reify Render

      (render [_] (str "M(" id ")"))

      Macro

      (macro-id [_] id)

      (macro-source [_]
        (str "MACRO("
             (->> terms
                  (map render)
                  (str/join ", "))
             ", END)")))))

(defn two-keys
  [modifier first-key second-key]
  ;; macros are tricky, you can't directly use key combinations, you have
  ;; to be explicit about UP and DOWN with a simple (unmodified) keycode.
  ;; TYPE is just DOWN then UP.
  ;; Reading the documentation, you see examples like T(A) ...
  ;; which is translated to TYPE(KC_A).
  (macro (fn-call "DOWN" modifier)
         (fn-call "TYPE" first-key)
         (fn-call "UP" modifier)
         (fn-call "TYPE" second-key)))

(defn alt-z
  [key]
  (two-keys :lalt :z key))

(defn ctrl-w
  [key]
  (two-keys :lctrl :w key))

(defn momentary
  "Change layer while key held (like shift key)"
  [layer]
  (fn-call "MO" layer))

(defn one-shot
  "Activate layer just for next key"
  [layer] (fn-call "OSL" layer))

(defn toggle
  "Toggle layer until layer is toggled again (like caps lock)"
  [layer] (fn-call "TG" layer))


(defn macro-support
  [keymaps]
  (let []
    (println
      "const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt)
{
  if (record->event.pressed) {
    switch(id) {")

    (doseq [m (->> keymaps
                   (mapcat vals)
                   (filter #(extends? Macro (class %))))]
      (println (str "      case " (macro-id m) ": return "
                    (macro-source m) ";")))

    (println "    }
  }
  return MACRO_NONE;
};")))

(defn keymaps [& keymaps]
  (println "const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {")
  (doseq [km keymaps]
    (print (keymap km))
    (println ","))
  (println "};\n")

  (macro-support keymaps))

(defn write-keymaps
  "Writes the keymap data to *out*.  Normally, this is written to the file keymap-gen.h."
  []
  (keymaps (merge (map-invert key*)
                  {:k56     :lalt
                   :k46     :lctrl
                   :k30     :caps
                   :k20     :lshift
                   :k26     :lgui
                   lh-left  :left
                   lh-right :right

                   :k10     (momentary 3)                   ; Clojure key
                   :k06     (momentary 1)                   ; fn key

                   :k57     :ralt
                   :k47     :rctrl
                   :k27     :rgui
                   :k2d     :rshift
                   :k1d     (momentary 3)                   ; Clojure key
                   :k07     (momentary 1)})                 ; fn key
           ;; Layer 1: Function keys and A/V controls
           {(key 1)      :f1
            (key 2)      :f2
            (key 3)      :f3
            (key 4)      :f4
            (key 5)      :f5
            (key 6)      :f6
            (key 7)      :f7
            (key 8)      :f8
            (key 9)      :f9
            (key 0)      :f10
            (key p)      :f11
            (key scolon) :f12
            (key slash)  :f13
            (key right)  :f14

            (key i)      :up
            (key j)      :left
            (key k)      :down
            (key l)      :right

            (key w)      :ms-up
            (key a)      :ms-left
            (key s)      :ms-down
            (key d)      :ms-right
            (key q)      :ms-btn1
            (key e)      :ms-btn2

            (key m)      :audio-mute
            (key comma)  :audio-vol-down
            (key dot)    :audio-vol-up

            (key c)      :media-play-pause
            (key v)      :media-prev-track
            (key b)      :media-next-track

            ;; The backtick key is a bit hard to reach, this make fn-escape
            ;; produce a backtick as well.
            (key esc)    :grave

            ;; Layer 2 is the numeric keypad
            (key equal)  (toggle 2)}
           ;; Layer 2: Numeric Keypad
           {(key esc)    "RESET"                            ; Easier than using a paper clip

            (key u)      :kp-7
            (key i)      :kp-8
            (key o)      :kp-9
            (key j)      :kp-4
            (key k)      :kp-5
            (key l)      :kp-6
            (key m)      :kp-1
            (key comma)  :kp-2
            (key dot)    :kp-3
            (key up)     :kp-dot
            (key 8)      :kp-slash
            (key 9)      :kp-asterisk
            (key 0)      :kp-minus
            (key p)      :kp-plus
            (key scolon) :kp-plus
            (key slash)  :kp-enter
            (key right)  :kp-enter
            (key space)  :kp-0}
           ;; Layer 3: Cursive/IntelliJ
           {(key v)        (ctrl-alt :d)                    ; move forward into sexp
            (key r)        (ctrl-alt :n)                    ; move forward out of sexp
            (key c)        (ctrl-alt :p)                    ; move backward into sexp
            (key d)        (ctrl-alt :b)                    ; move backward
            (key e)        (ctrl-alt :u)                    ; move backward out of sexp
            (key f)        (ctrl-alt :f)                    ; move forward
            (key lbracket) (alt-gui :left)                  ; navigate / back
            (key rbracket) (alt-gui :right)                 ; navigate / forward
            (key bspace)   (ctrl-shift :c)                  ; clear repl output
            (key tab)      (ctrl-shift :t)                  ; run tests in ns in repl
            (key esc)      (gui-shift :bspace)              ; clear all test markers
            (key q)        (ctrl-alt-gui :t)                ; run test under cursor in repl
            (key z)        (ctrl-shift :e)                  ; send form before carat to repl
            (key a)        (ctrl-shift :n)                  ; switch repl ns to current file
            (key 2)        (gui :f12)                       ; file structure
            (key 1)        (alt :f1)                        ; navigate / select in ...
            (key enter)    (ctrl-shift :m)                  ; load file in repl
            (key space)    (ctrl-shift :o)                  ; [open] namespace ...
            (key w)        (ctrl-shift :lbracket)           ; barf backwards
            (key s)        (ctrl-gui :j)                    ; slurp backwards
            (key j)        (alt-shift :s)                   ; split
            (key t)        (ctrl-shift :rbracket)           ; barf forwards
            (key g)        (ctrl-shift :0)                  ; slurp forwards
            (key b)        (ctrl-gui :s)                    ; join
            (key k)        (alt :s)                         ; splice
            (key comma)    (alt-gui :comma)                 ; thread form
            (key dot)      (alt-gui :dot)                   ; unthread form
            (key p)        (gui-shift :a)                   ; find action ...
            (key 6)        (shift :f6)                      ; rename ...
            (key scolon)   (alt-gui :l)                     ; reformat
            (key quote)    (gui :quote)                     ; raise
            (key x)        (gui :quote)                     ; raise
            (key 7)        (alt :f7)                        ; find usages
            (key up)       (alt-gui :up)                    ; previous occurance (search)
            (key down)     (alt-gui :down)                  ; next occurance (search)
            (key y)        (ctrl-shift :j)                  ; join lines
            (key minus)    (ctrl-w :p)                      ; pin active tab
            (key home)     (alt-z :left)                    ; last edit location
            (key end)      (alt-z :right)                   ; next edit location
            lh-left        (ctrl-w :left)                   ; select previous tab
            lh-right       (ctrl-w :right)                  ; select next tab
            (key left)     (gui-shift :up)                  ; move form up
            (key right)    (gui-shift :down)}))             ; move form down
