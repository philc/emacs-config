;;; -*- lexical-binding: t; -*-
;;
;; Workspaces: two levels of tabs: a workspace corresponds to a project, and then there can be one
;; or more tabs in a project's workspace. This mirrors my tmux setup.
;;
;; Here a workspace is a tab-bar group: every tab belongs to one workspace, named by the tab's
;; `group' parameter. New tabs join the current tab's workspace (`tab-bar-new-tab-group' is t by
;; default), and desktop-save-mode saves and restores each tab's group. Tabs are per frame, so
;; workspaces are too.
;;
;; The tab bar shows two rows: every workspace, sorted by name, and then the current workspace's
;; tabs.
;;
;; There are two ways to get to a project, for two different intents:
;; - `ws/picker' (Cmd-P) goes to a project's workspace: it jumps to any tab in any workspace, or
;;   creates a workspace for a project which doesn't have one yet.
;; - `ws/open-project' (leader vp) opens a project in the current window, without changing
;;   workspaces. This is for looking at another project from the one I'm working in, e.g. reading
;;   my tmux scripts in ~/sys while working on this Emacs config.
(provide 'workspaces)
(require 'cl-lib)
(require 'seq)
(require 'tab-bar)
(require 'evil-ext)

(defvar ws/project-dirs '("~/projects/*/" "~/projects/claude/*/" "~/sys/")
  "Wildcards for my project directories. These are the same as in
   ~/sys/scripts/tmux/tmux_workspace_picker.sh.")

(defun ws/project-paths ()
  "The paths of my project directories, expanded from `ws/project-dirs'."
  (mapcar #'directory-file-name
          (mapcan (lambda (w) (file-expand-wildcards (expand-file-name w)))
                  ws/project-dirs)))

(defvar ws/default-name "misc"
  "The workspace of tabs which have no group, such as the first tab of a new frame.")

;;
;; Reading the tab list.
;;

(defun ws/tabs ()
  (funcall tab-bar-tabs-function))

(defun ws/workspace (tab)
  "The name of TAB's workspace."
  (or (alist-get 'group tab) ws/default-name))

(defun ws/current-tab-p (tab)
  (eq (car tab) 'current-tab))

(defun ws/current (tabs)
  "The name of the current tab's workspace."
  (ws/workspace (seq-find #'ws/current-tab-p tabs)))

(defun ws/names (tabs)
  "The names of the workspaces in TABS, sorted."
  (sort (delete-dups (mapcar #'ws/workspace tabs)) #'string<))

(defun ws/positions (tabs name)
  "The 0-based positions in TABS of the tabs in workspace NAME, in tab order."
  (cl-loop for tab in tabs
           for i from 0
           when (equal (ws/workspace tab) name)
           collect i))

(defun ws/current-position (tabs)
  (cl-position-if #'ws/current-tab-p tabs))

(defun ws/tab-time (tab)
  "When TAB was last selected. The current tab has no `time', since it's selected now."
  (if (ws/current-tab-p tab)
      1.0e+INF
    (or (alist-get 'time tab) 0)))

(defun ws/most-recent-position (tabs name)
  "The position of the most recently used tab in workspace NAME."
  (car (seq-sort-by (lambda (i) (ws/tab-time (nth i tabs)))
                    #'>
                    (ws/positions tabs name))))

(defun ws/workspace-time (tabs name)
  "When workspace NAME was last used: the time of its most recently used tab."
  (ws/tab-time (nth (ws/most-recent-position tabs name) tabs)))

(defun ws/cycle (list current delta)
  "The element DELTA places after CURRENT in LIST, wrapping around."
  (let ((i (cl-position current list :test #'equal)))
    (nth (mod (+ i delta) (length list)) list)))

;;
;; Switching tabs and workspaces.
;;

(defun ws/select-position (i)
  "Select the tab at 0-based position I."
  (tab-bar-select-tab (1+ i))
  (evil-ext/switch-to-normal-state))

(defun ws/switch-tab (delta)
  "Select the tab DELTA tabs away within the current workspace, wrapping around."
  (let* ((tabs (ws/tabs))
         (positions (ws/positions tabs (ws/current tabs))))
    (ws/select-position (ws/cycle positions (ws/current-position tabs) delta))))

(defun ws/next-tab ()
  "Select the next tab in the current workspace."
  (interactive)
  (ws/switch-tab 1))

(defun ws/prev-tab ()
  "Select the previous tab in the current workspace."
  (interactive)
  (ws/switch-tab -1))

(defun ws/switch-to (name)
  "Select the most recently used tab of workspace NAME."
  (ws/select-position (ws/most-recent-position (ws/tabs) name)))

(defun ws/switch-workspace (delta)
  "Switch to the workspace DELTA workspaces away, in name order, wrapping around."
  (let ((tabs (ws/tabs)))
    (ws/switch-to (ws/cycle (ws/names tabs) (ws/current tabs) delta))))

(defun ws/next-workspace ()
  "Switch to the next workspace, in name order."
  (interactive)
  (ws/switch-workspace 1))

(defun ws/prev-workspace ()
  "Switch to the previous workspace, in name order."
  (interactive)
  (ws/switch-workspace -1))

;;
;; Creating and closing.
;;

(defun ws/open-project-root (dir)
  "Show project DIR's README.md in the current window, or dired if it has none."
  (let ((readme (expand-file-name "README.md" dir)))
    (if (file-exists-p readme)
        (find-file readme)
      (dired dir))))

(defun ws/create (name &optional dir)
  "Open a tab in a new workspace NAME, showing project DIR (see `ws/open-project-root'), or dired in
   ~ if DIR isn't given."
  ;; The new tab goes at the end, so it doesn't land in the middle of another workspace's tabs.
  (let ((tab-bar-new-tab-to 'rightmost))
    (tab-bar-new-tab))
  (tab-bar-change-tab-group name)
  (if dir
      (ws/open-project-root dir)
    (dired "~"))
  (evil-ext/switch-to-normal-state))

(defun ws/switch-or-create (name)
  (if (member name (ws/names (ws/tabs)))
      (ws/switch-to name)
    (ws/create name)))

(defun ws/close-tab ()
  "Close the current tab. If it was its workspace's last tab, switch to the next workspace, as tmux
   does when a session's last window closes. Otherwise, select the tab to its right within the
   workspace, or to its left if it was the last one."
  (interactive)
  (let* ((tabs (ws/tabs))
         (name (ws/current tabs))
         (k (cl-position (ws/current-position tabs) (ws/positions tabs name)))
         (next (ws/cycle (ws/names tabs) name 1)))
    (tab-bar-close-tab)
    (let ((positions (ws/positions (ws/tabs) name)))
      (if positions
          (ws/select-position (nth (min k (1- (length positions))) positions))
        (ws/switch-to next)))))

;;
;; The tab bar.
;;

(defun ws/format-item (text current-p)
  (propertize (format " %s " text)
              'face (if current-p 'tab-bar-tab 'tab-bar-tab-inactive)))

(defun ws/format-tab-bar ()
  "The two rows of the tab bar: the workspaces, then the current workspace's tabs, numbered within
   the workspace. This is the only item in `tab-bar-format'. A newline in the tab bar's text starts
   a new row."
  (let* ((tabs (ws/tabs))
         (current (ws/current tabs)))
    (concat
     (mapconcat (lambda (name) (ws/format-item name (equal name current)))
                (ws/names tabs)
                "")
     "\n"
     (cl-loop for i in (ws/positions tabs current)
              for n from 1
              for tab = (nth i tabs)
              concat (ws/format-item (format "%d %s" n (alist-get 'name tab))
                                     (ws/current-tab-p tab))))))

;;
;; The picker.
;;

(defun ws/picker-candidates (tabs dirs)
  "The picker's entries, as an alist of (label . action), where action is a function of no
   arguments. First, every tab except the current one, with the most recently used workspaces
   first, labeled \"workspace  N:tab\". Then the DIRS which don't have a workspace yet, labeled
   \"+ name  ~/path\"."
  (let* ((names (ws/names tabs))
         (by-recency (seq-sort-by (lambda (name) (ws/workspace-time tabs name)) #'> names))
         (tab-entries
          (cl-loop for name in by-recency
                   nconc (cl-loop for i in (ws/positions tabs name)
                                  for n from 1
                                  for tab = (nth i tabs)
                                  unless (ws/current-tab-p tab)
                                  collect (cons (format "%s  %d:%s" name n (alist-get 'name tab))
                                                (lambda () (ws/select-position i))))))
         (dir-entries
          (mapcan (lambda (dir)
                    (let ((name (file-name-nondirectory dir)))
                      (unless (member name names)
                        (list (cons (format "+ %s  %s" name (abbreviate-file-name dir))
                                    (lambda () (ws/create name dir)))))))
                  dirs)))
    (append tab-entries dir-entries)))

(defvar ws/picker--create-named nil
  "Set by `ws/picker-create-named' to tell `ws/picker' to create a workspace named the input.")

(defun ws/picker-create-named ()
  "Close the picker and create (or switch to) a workspace named exactly what was typed. This exits
   with `exit-minibuffer', which, unlike Enter, doesn't require the input to match an entry."
  (interactive)
  (setq ws/picker--create-named t)
  (exit-minibuffer))

(defvar ws/picker-map
  (let ((map (make-sparse-keymap)))
    ;; Option-Enter, as Alt-Enter in the tmux picker. Option is the alt modifier in my config.
    (define-key map (kbd "A-RET") #'ws/picker-create-named)
    (define-key map (kbd "A-<return>") #'ws/picker-create-named)
    map))

(defun ws/picker ()
  "Jump to a tab in any workspace, or create a workspace for a project directory. Option-Enter
   creates a workspace named what was typed, in ~."
  (interactive)
  (let* ((candidates (ws/picker-candidates (ws/tabs) (ws/project-paths)))
         (labels (mapcar #'car candidates))
         ;; Keep the candidates in the order given, rather than letting vertico sort them.
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action labels string pred))))
         ;; Match substrings, as the tmux picker does, rather than fuzzily.
         (completion-styles '(substring))
         (completion-ignore-case t)
         (ws/picker--create-named nil)
         (choice (minibuffer-with-setup-hook
                     ;; :append, so this runs after vertico's setup hook, and this map takes
                     ;; priority over vertico's.
                     (:append (lambda ()
                                (use-local-map (make-composed-keymap ws/picker-map
                                                                     (current-local-map)))))
                   ;; Requiring a match makes Enter do nothing when nothing matches, so a typo
                   ;; doesn't do anything.
                   (completing-read "> " table nil t))))
    (if ws/picker--create-named
        (let ((name (string-trim choice)))
          (unless (string-empty-p name)
            (ws/switch-or-create name)))
      (when-let* ((action (cdr (assoc choice candidates))))
        (funcall action)))))

(defun ws/open-project ()
  "Open a project in the current window (see `ws/open-project-root'), without changing workspaces."
  (interactive)
  (let* ((dirs (mapcar (lambda (dir)
                         (cons (format "%s  %s" (file-name-nondirectory dir)
                                       (abbreviate-file-name dir))
                               dir))
                       (ws/project-paths)))
         (completion-styles '(substring))
         (completion-ignore-case t)
         (choice (completing-read "Project: " dirs nil t)))
    (ws/open-project-root (cdr (assoc choice dirs)))
    (evil-ext/switch-to-normal-state)))
