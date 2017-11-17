(defn my-pst
  "Pretty-prints a stacktrace, like `pst`, but and ignores some frames."
  ; See the implementation of clojure.repl/pst for reference:
  ; https://github.com/clojure/clojure/blob/master/src/clj/clojure/repl.clj
  ([]
   (my-pst *e false))
  ; The omit-message flag is useful because nREPL prints the exception message when it gets thrown, but not
  ; the exception backtrace, so it's useful to be able to print just the backtrace.
  ([e omit-message]
   (if-not e
     (println "This exception is nil.")
     (do
       (println "")
       (if omit-message
         ; NOTE(philc): I'm printing this single non-whitespace character because otherwise, tab indentation
         ; doesn't work when piped through nrepl to Emacs. I think this is because in Emacs, I'm stripping off
         ; the leading whitespace from REPL output.
         (print ".")
         (println (str (-> e class .getSimpleName) " "
                       (.getMessage e)
                       (when-let [info (ex-data e)] (str " " (pr-str info))))))
       (let [depth 100 ; in pst's implementation, the default is 12.
             root (clojure.stacktrace/root-cause e)
             st (.getStackTrace root)
             excluded-fns ["clojure.main$repl"
                           "clojure.lang" ; Excluding all of clojure.lang may be aggressive
                           "clojure.core"
                           "clojure.tools.nrepl"
                           "clojure.core.protocols"
                           "java.util.concurrent"
                           "java.lang"]
             exclude-frame? (fn [element]
                              (some #(.contains (.getClassName element) %)
                                    excluded-fns))]
         (doseq [el (->> st
                         (remove exclude-frame?)
                         (take depth))]
           (println #_(.getClassName el)
                    (str \tab (clojure.repl/stack-element-str el)))))))))

; Make this function avaiable from any namespace.
(intern 'clojure.core 'my-pst my-pst)
