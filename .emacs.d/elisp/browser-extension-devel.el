;; Commands for reloading a browser extension in either Chrome or Firefox during development.

(provide 'browser-extension-devel)

(setq ext-dev/browser-app "Firefox")

(defun ext-dev/set-browser-app-to-firefox ()
  (interactive)
  (setq ext-dev/browser-app "Firefox")
  "")

(defun ext-dev/set-browser-app-to-chrome ()
  (interactive)
  (setq ext-dev/browser-app "Google Chrome Beta")
  "")

(defun ext-dev/reload-extension-in-browser ()
  (interactive)
  (util/save-buffer-if-dirty)
  (pcase ext-dev/browser-app
    ("Firefox" (ext-dev/reload-firefox))
    ("Google Chrome Beta" (ext-dev/reload-chrome))
    (_ (print "Unknown browser")))
  "")

(defun ext-dev/reload-firefox ()
  "Reloads the extension in Firefox."
  (let* (;; This corresponds to the "Internal UUID" field show in about:addons for the extension.
         ;; Sometimes this internal extension ID can change as the extension's manifest.json changes.
         (extension-id "bb3cce7b-03e2-4751-840e-265d62e4d9f0")
         ;; This URL is specific to the Vimium extension.
         (url (format "moz-extension://%s/pages/reload.html" extension-id)))
    (util/call-process-with-exit-status "open"
                                        nil
                                        ;; Which app to open
                                        "-a"
                                        "Firefox"
                                        ;; -g means don't foreground the app.
                                        "-g"
                                        url)))

(defun ext-dev/reload-chrome ()
  "Reloads the extension in Chrome."
  (let* (;; This corresponds to the "ID" field show in chrome://extensions for the extension.
         ;; Sometimes this internal extension ID can change if the extension is unloaded and
         ;; reinstalled.
         (extension-id "cmodefmaanjcofhjjhilnkigegimcdda")
         ;; This URL is specific to the Vimium extension.
         (url (format "chrome-extension://%s/pages/reload.html" extension-id)))
    (util/call-process-with-exit-status
     "open"
     nil
     "-a"
     ext-dev/browser-app
     ;; -g means don't foreground the app and don't take the keyboard focus, but Chrome ignores
     ;; -this. See this longstanding bug: https://issues.chromium.org/issues/40581582
     "-g"
     url)
    ;; Since Chrome steals focus, re-focus Emacs. Empirically, this must be done in a run-with-timer
    ;; invocation, rather than immediately after the previous `open` call.
    (let (; Get the name of the currently running Emacs app.
          (emacs-mac-app (->> (expand-file-name invocation-name invocation-directory)
                              (s-split "/")
                              (-find (lambda (s) (s-ends-with? ".app" s))))))
      (run-with-timer
       0
       nil
       (lambda (app) (util/call-process-with-exit-status "open" nil "-a" app))
       emacs-mac-app))
    ;; We use applescript to reload all of Chrome's tabs in the front window, because this can't be
    ;; done reliably by the extension itself while it's being unloaded. Otherwise, the current tab
    ;; will have outdated and orphaned content scripts.
    (run-with-timer
     ;; This must be invoked with a delay after reload.html is called, presumably because Chrome is
     ;; still reloading the extension. If done with a smaller delay than 0.3, the tab will reload,
     ;; but the extension will not yet be reloaded, and no content scripts will be present.
     0.3
     nil
     (lambda ()
       ;; The single line version is:
       ;; "tell application \"Google Chrome Beta\" to tell front window to reload active tab")
       ;; I couldn't get osasccript to accept this as a single line argument, even though it appears
       ;; syntactically valid, so I've instead split it across multiple lines.
       (util/call-process-with-exit-status
        "osascript"
        nil
        "-e" (format "tell application \"%s\"" ext-dev/browser-app)
        "-e" "repeat with theTab in tabs of front window"
        "-e" "reload theTab"
        "-e" "end repeat"
        "-e" "end tell")))))
