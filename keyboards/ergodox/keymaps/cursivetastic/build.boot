(set-env! :resource-paths #{"."})

(require
  '[keymapping :refer [write-to-file]])

;; Indentation via Cursive is all weird because it's not a proper Clojure file.

(deftask write-keymap
         "Writes the keymap to a file."
         [f file PATH file "Output file."]

         (write-to-file file))
