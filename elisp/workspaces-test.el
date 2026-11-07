;;; -*- lexical-binding: t; -*-
;;
;; Unit tests for workspaces.el.
;;
;; Run these with `make test`.

(require 'ert)
(require 'cl-lib)
(require 'workspaces)

(defun ws-test/state ()
  "The current workspace and the names of its tabs, with the current tab marked with *."
  (let ((tabs (ws/tabs)))
    (cons (ws/current tabs)
          (mapcar (lambda (i)
                    (let ((tab (nth i tabs)))
                      (concat (if (ws/current-tab-p tab) "*" "") (alist-get 'name tab))))
                  (ws/positions tabs (ws/current tabs))))))

;; This uses the real tab bar, in the batch Emacs's frame.
(ert-deftest ws-test/commands ()
  (tab-bar-mode 1)
  (unwind-protect
      (progn
        (tab-bar-rename-tab "m1")
        (ws/create "beta")
        (tab-bar-rename-tab "b1")
        (tab-bar-new-tab)
        (tab-bar-rename-tab "b2")
        (ws/create "alpha")
        (tab-bar-rename-tab "a1")
        (should (equal (ws-test/state) '("alpha" "*a1")))
        (should (equal (substring-no-properties (ws/format-tab-bar))
                       " alpha  beta  misc \n 1 a1 "))
        ;; Workspaces cycle in name order, landing on the most recently used tab.
        (ws/next-workspace)
        (should (equal (ws-test/state) '("beta" "b1" "*b2")))
        (ws/next-tab)
        (should (equal (ws-test/state) '("beta" "*b1" "b2")))
        (ws/next-tab)
        (should (equal (ws-test/state) '("beta" "b1" "*b2")))
        (ws/prev-workspace)
        (should (equal (ws-test/state) '("alpha" "*a1")))
        (ws/prev-workspace)
        (should (equal (ws-test/state) '("misc" "*m1")))
        ;; Closing a workspace's last tab switches to the next workspace.
        (ws/switch-to "alpha")
        (ws/close-tab)
        (should (equal (ws-test/state) '("beta" "b1" "*b2")))
        ;; Closing a tab with others in its workspace stays in the workspace.
        (ws/prev-tab)
        (ws/close-tab)
        (should (equal (ws-test/state) '("beta" "*b2"))))
    (tab-bar-close-other-tabs)
    (tab-bar-change-tab-group "")
    (tab-bar-mode -1)))

(defun ws-test/tab (name group time &optional current)
  "A tab, as in the list returned by `tab-bar-tabs-function'."
  `(,(if current 'current-tab 'tab) (name . ,name) (group . ,group) (time . ,time)))

(ert-deftest ws-test/picker-candidates ()
  (let* ((tabs (list (ws-test/tab "a1" "alpha" 10)
                     (ws-test/tab "b1" "beta" 30)
                     (ws-test/tab "a2" "alpha" 20 t)
                     (ws-test/tab "b2" "beta" 40)
                     (ws-test/tab "c1" "gamma" 35)))
         (labels (mapcar #'car (ws/picker-candidates tabs '("/p/alpha" "/p/delta")))))
    ;; The current workspace first, without the current tab; then the others by recency; then the
    ;; projects without a workspace.
    (should (equal labels '("alpha  1:a1" "beta  1:b1" "beta  2:b2" "gamma  1:c1"
                            "+ delta  /p/delta")))))
