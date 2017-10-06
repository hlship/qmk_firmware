(ns cursivetastic.keymapping
  (:require
    fs
    [clojure.string :as str]
    [clojure.set :refer [map-invert]]))

(def keycode-pos
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
               lshift   k20
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
               rshift   k2d
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

(defn as-keyword [x]
  (if (keyword? x)
    x
    (-> x str keyword)))

(defn keycode [x]
  (let [keycode (key* (as-keyword x))]
    (or keycode
        (throw (ex-info (str "Bad param to keycode: " (pr-str x))
                        {:key        x
                         :valid-keys (keys key*)})))))

(defprotocol Render
  (render [x]))

(extend-protocol Render

  Keyword
  (render [keycode]
    (str "KC_" (-> keycode
                   name
                   str/upper-case
                   (str/replace "-" "_"))))

  string
  (render [s] s)

  number
  (render [n] (str n)))

(defrecord FnCall [fn-name wrapped]
  Render
  (render [_]
    (str fn-name "(" (render wrapped) ")")))

(def fn-call ->FnCall)

(defn keymap->lines
  [keymap]
  (let [ordered (reduce-kv
                  (fn [args keycode v]
                    (let [p (keycode-pos keycode)]
                      (when (nil? p)
                        (throw (ex-info (str "Bad keymap key. " (pr-str keycode))
                                        {:key    keycode
                                         :value  v
                                         :keymap keymap})))
                      (assoc args p (render v))))
                  (vec (repeat (count keycode-pos) "KC_TRNS"))
                  keymap)]
    ;; KEYMAP is a macro from ez.h that converts its arguments into nested arrays
    ;; with an even more arcane ordering.
    (str "LAYOUT_ergodox("
         (str/join ",\n  " ordered)
         ")")))

(defn ctrl [x] (fn-call "LCTL" x))
(defn alt [x] (fn-call "LALT" x))
(defn gui [x] (fn-call "LGUI" x))
(defn shift [x] (fn-call "LSFT" x))
(defn ctrl-alt-gui [x] (fn-call "LCAG" x))

(def ctrl-shift (comp shift ctrl))
(def ctrl-alt (comp ctrl alt))
(def ctrl-alt-shift (comp ctrl alt shift))
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
  [keycode]
  (two-keys :lalt :z keycode))

(defn ctrl-w
  [keycode]
  (two-keys :lctrl :w keycode))

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


(defn keymap-file-content
  [keymaps]
  (let [keymap-lines (->> keymaps
                          (map keymap->lines)
                          (str/join ",\n"))
        macro-lines (for [m (->> keymaps
                                 (mapcat vals)
                                 (filter #(satisfies? Macro %))
                                 (sort-by macro-id))]
                      (str "      case " (macro-id m) ": return "
                           (macro-source m) ";\n"))]
    (str "const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {\n"
         keymap-lines
         "\n};\n\n"
         "const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt)
{
  if (record->event.pressed) {
    switch(id) {\n"
         (str/join macro-lines)
         "    }
  }
  return MACRO_NONE;
};")))

(def keymaps
  [(merge (map-invert key*)
          {:k56     :lalt
           :k46     :lctrl
           :k30     :caps
           :k26     :lgui
           lh-left  :left
           lh-right :right

           :k10     (momentary 3)                           ; Clojure key
           :k06     (momentary 1)                           ; fn key

           :k57     :ralt
           :k47     :rctrl
           :k27     :rgui
           :k1d     (momentary 3)                           ; Clojure key
           :k07     (momentary 1)})                         ; fn key
   ;; Layer 1: Function keys and A/V controls
   {(keycode 1)       :f1
    (keycode 2)       :f2
    (keycode 3)       :f3
    (keycode 4)       :f4
    (keycode 5)       :f5
    (keycode 6)       :f6
    (keycode 7)       :f7
    (keycode 8)       :f8
    (keycode 9)       :f9
    (keycode 0)       :f10
    (keycode \p)      :f11
    (keycode :scolon) :f12
    (keycode :slash)  :f13
    (keycode :right)  :f14

    (keycode \i)      :up
    (keycode \j)      :left
    (keycode \k)      :down
    (keycode \l)      :right

    (keycode \w)      :ms-up
    (keycode \a)      :ms-left
    (keycode \s)      :ms-down
    (keycode \d)      :ms-right
    (keycode \q)      :ms-btn1
    (keycode \e)      :ms-btn2

    (keycode \m)      :audio-mute
    (keycode :comma)  :audio-vol-down
    (keycode :dot)    :audio-vol-up

    (keycode \c)      :media-play-pause
    (keycode \v)      :media-prev-track
    (keycode \b)      :media-next-track

    ;; The backtick key is a bit hard to reach, this make fn-escape
    ;; produce a backtick as well.
    (keycode :esc)    :grave

    ;; Likewise, easy way to create a pipe char:
    (keycode :space)  :pipe

    ;; This is really for Elixir coding: The |> operator

    ;; This might be something that ca be done easier ...
    (keycode :enter)  (macro (fn-call "DOWN" :lshift)
                             (fn-call "TYPE" :bslash)
                             (fn-call "TYPE" :dot)
                             (fn-call "UP" :lshift))

    ;; Layer 2 is the numeric keypad
    (keycode :equal)  (toggle 2)}
   ;; Layer 2: Numeric Keypad
   {(keycode :esc)    "RESET"                               ; Easier than using a paper clip

    (keycode \u)      :kp-7
    (keycode \i)      :kp-8
    (keycode \o)      :kp-9
    (keycode \j)      :kp-4
    (keycode \k)      :kp-5
    (keycode \l)      :kp-6
    (keycode \m)      :kp-1
    (keycode :comma)  :kp-2
    (keycode :dot)    :kp-3
    (keycode :up)     :kp-dot
    (keycode 8)       :kp-slash
    (keycode 9)       :kp-asterisk
    (keycode 0)       :kp-minus
    (keycode \p)      :kp-plus
    (keycode :scolon) :kp-plus
    (keycode :slash)  :kp-enter
    (keycode :right)  :kp-enter
    (keycode :space)  :kp-0}
   ;; Layer 3: Cursive/IntelliJ
   {(keycode \v)        (ctrl-alt :d)                       ; move forward into sexp
    (keycode \r)        (ctrl-alt :n)                       ; move forward out of sexp
    (keycode \c)        (ctrl-alt :p)                       ; move backward into sexp
    (keycode \d)        (ctrl-alt :b)                       ; move backward
    (keycode \e)        (ctrl-alt :u)                       ; move backward out of sexp
    (keycode \f)        (ctrl-alt :f)                       ; move forward
    (keycode :lbracket) (alt-gui :left)                     ; navigate / back
    (keycode :rbracket) (alt-gui :right)                    ; navigate / forward
    (keycode :bspace)   (ctrl-shift :c)                     ; clear repl output
    (keycode :tab)      (ctrl-shift :t)                     ; run tests in ns in repl
    (keycode :esc)      (gui-shift :bspace)                 ; clear all test markers
    (keycode \q)        (ctrl-alt-gui :t)                   ; run test under cursor in repl
    (keycode \z)        (ctrl-shift :e)                     ; send form before carat to repl
    (keycode \a)        (ctrl-shift :n)                     ; switch repl ns to current file
    (keycode 2)         (gui :f12)                          ; file structure
    (keycode 1)         (alt :f1)                           ; navigate / select in ...
    (keycode :enter)    (ctrl-shift :m)                     ; load file in repl
    (keycode :space)    (ctrl-shift :o)                     ; [open] namespace ...
    (keycode \w)        (ctrl-shift :lbracket)              ; barf backwards
    (keycode \s)        (ctrl-gui :j)                       ; slurp backwards
    (keycode \j)        (alt-shift :s)                      ; split
    (keycode \t)        (ctrl-shift :rbracket)              ; barf forwards
    (keycode \g)        (ctrl-shift :0)                     ; slurp forwards
    (keycode \b)        (ctrl-gui :s)                       ; join
    (keycode \k)        (alt :s)                            ; splice
    (keycode :delete)   (ctrl-alt :k)                       ; kill sexp
    (keycode :comma)    (alt-gui :comma)                    ; thread form
    (keycode :dot)      (alt-gui :dot)                      ; unthread form
    (keycode \p)        (gui-shift :a)                      ; find action ...
    (keycode 6)         (shift :f6)                         ; rename ...
    (keycode :scolon)   (alt-gui :l)                        ; reformat
    (keycode :quote)    (gui :quote)                        ; raise
    (keycode \x)        (gui :quote)                        ; raise
    (keycode 7)         (alt :f7)                           ; find usages
    (keycode :up)       (alt-gui :up)                       ; previous occurrence (search)
    (keycode :down)     (alt-gui :down)                     ; next occurrence (search)
    (keycode \y)        (ctrl-shift :j)                     ; join lines
    (keycode :minus)    (ctrl-w :p)                         ; pin active tab
    (keycode :home)     (alt-z :left)                       ; last edit location
    (keycode :end)      (alt-z :right)                      ; next edit location
    (keycode :pgup)     (ctrl-alt-shift :up)                ; previous change
    (keycode :pgdown)   (ctrl-alt-shift :down)              ; next change
    lh-left             (ctrl-w :left)                      ; select previous tab
    lh-right            (ctrl-w :right)                     ; select next tab
    (keycode :lshift)   (ctrl-w :n)                         ; goto prev splitter
    (keycode :rshift)   (ctrl-w :m)                         ; goto next splitter
    (keycode :left)     (gui-shift :up)                     ; move form up
    (keycode :right)    (gui-shift :down)}                  ; move form down
   ])

(fs/writeFileSync "gen-keymap.h" (keymap-file-content keymaps))

(println "Wrote gen-keymap.h")


