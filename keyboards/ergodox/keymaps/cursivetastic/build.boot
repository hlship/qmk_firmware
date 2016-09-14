(set-env! :resource-paths #{"."})

(require
  '[clojure.java.io :as io]
  '[keymapping :refer [write-keymaps]])

;; Indentation via Cursive is all weird because it's not a proper Clojure file.

(deftask write-keymap
         "Writes the keymap to a file."
         [f file PATH file "Output file."]

         (println "Writing to:" (.getPath file))

         (binding [*out* (io/writer file)]
           (write-keymaps)))
