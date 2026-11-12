;;; -*- lexical-binding: t; -*-
;;
;; Diff review: leave code review comments on the lines of a commit, in magit's revision buffer
;; (the view of one commit, opened from the log), like a PR review on GitHub, but local. This is for
;; reviewing the stacks of commits Claude makes: I comment on each commit, and Claude folds the
;; fixes into the commits they belong to, so the stack ends up shaped the way I want.
;;
;; Comments are stored in .git/review.json, which Claude reads. Being inside .git, it's never
;; committed and never shows as untracked.
;;
;; Design decisions, and why:
;; - Comments are per commit, rather than on the combined diff of a branch, so that each fix can be
;;   folded into its own commit.
;; - A comment is typed inline, below the line it's about, as if the diff were a text file, rather
;;   than in the minibuffer, so that it can be paragraphs long.
;; - Comments are real text in the buffer, not overlays, so that normal-state commands work inside
;;   them: moving to any line and column, dd, ciw, p, and so on.
;; - A comment is saved as soon as I'm out of insert state, whether by Esc or by a normal-state
;;   edit like dd. There's no explicit save or submit step.
;; - Besides comments on a line, there are comments on a whole file (`o' on its heading) and on the
;;   whole commit (`o' in the header or message), for high-level remarks like "reformat every file
;;   in this commit", rather than leaving those on some arbitrary first line.
;; - Comments only appear in revision buffers (`o' on a commit in the log), not in the diffs in the
;;   status buffer.
;; - In a revision buffer, `o' opens a comment, like vim's open-line. RET visits the file, as RET
;;   opens a commit in the log. `O' does nothing on a diff line, since a comment always sits below
;;   the line it's about.
;; - Inside a comment, keys mean what they mean when editing text (e.g. `o' opens a new line of the
;;   comment), except for magit's section navigation (e.g. `gu'), which keeps working.
;; - Navigation shouldn't have rough edges: magit's section commands treat a comment as part of the
;;   hunk it's in, so they step over it, and folding a hunk folds its comments.
;;
;; How it works:
;; - On each refresh of a revision buffer, the comments for its commit are inserted into the text
;;   of the diff. A line comment goes right below its line, inside its hunk. Then all of the
;;   buffer's text except the comments is given the read-only text property, so edits can't spill
;;   from a comment into the diff.
;; - The buffer is read-only (as magit buffers are) except while point is in a comment. Then the
;;   buffer's local keymap is swapped for `dr/comment-map', so magit's keys don't shadow the normal
;;   editing keys.
;; - Commits get rewritten constantly as Claude reworks the stack, which changes their hashes, so a
;;   comment is matched to a commit by its hash, or failing that its patch-id (which survives a
;;   rebase if the commit's diff didn't change), or failing that its subject. Within the commit,
;;   a line comment is matched by its file, line number, and the line's text. If the line moved, it
;;   goes to the nearest line with that text. If it can't be found at all, the comment is shown
;;   with the file's comments (or the commit's, if the file is gone), with a dashed bar.
;; - Magit reads hunk text from the buffer in a couple of places, which comment text would throw
;;   off: visiting a file counts the lines of the hunk to find the file's line number, and applying
;;   or reversing a hunk turns its text into a patch. Visiting is advised to take the comments out
;;   of the buffer while it runs, and applying or reversing a hunk with comments is refused.
;;
;; A comment in .git/review.json looks like:
;;   {"id": "3f2a9c1e", "kind": "line", "commit": "<hash>", "subject": "link-hints: add ...",
;;    "patch_id": "<patch-id>", "file": "content_scripts/link_hints.js", "side": "new",
;;    "line": 142, "code": "    const text = el.innerText.trim();", "comment": "Use textContent..."}
;; "kind" is "line", "file", or "commit". "side" is "new" for an added or context line, whose "line"
;; is in the commit's version of the file, and "old" for a removed line, whose "line" is in the
;; parent's version.
(provide 'diff-review)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'text-property-search)
(require 'magit)
(require 'evil)

(defface dr/comment
  '((((background dark)) :background "#2a3238" :extend t)
    (t :background "#eef2f5" :extend t))
  "Face for the text of review comments."
  :group 'magit-faces)

(defface dr/comment-bar
  '((((background dark)) :foreground "#e9b96e")
    (t :foreground "#c4a000"))
  "Face for the bar to the left of review comments."
  :group 'magit-faces)

(defvar dr/comment-map (make-sparse-keymap)
  "The local keymap while point is in a comment. It has no parent, so that magit's keys don't
shadow evil's, and only the keys bound here, such as section navigation, keep a magit meaning.")

(defconst dr--bar (propertize "┃ " 'face 'dr/comment-bar)
  "Displayed before each line of a comment.")

(defconst dr--orphan-bar (propertize "┆ " 'face 'dr/comment-bar)
  "Displayed before each line of a comment whose line isn't in the diff anymore.")

(defconst dr--undo-commands '(undo undo-only undo-redo evil-undo evil-redo)
  "Commands which may restore a deleted comment, from a diff line.")

(defvar-local dr--commit nil
  "The plist (:sha :subject :patch-id) of the commit this buffer shows.")

(defvar-local dr--comments nil
  "A hash table from the id of each comment shown in this buffer to the comment's alist.")

(defvar-local dr--placements nil
  "A hash table from the id of each comment shown in this buffer to (SECTION . BAR): the magit
section its text belongs to, and the bar to display before its lines.")

(defvar-local dr--dirty nil
  "The ids of the comments whose text has changed since they were last saved.")

(defvar-local dr--changing nil
  "The ids of the comments a change in progress may affect. See `dr--before-change'.")

(defvar-local dr--chars-tick nil
  "`buffer-chars-modified-tick' when the change in progress began.")

(defvar-local dr--in-comment nil
  "Whether point was in a comment after the last command.")

(defvar-local dr--magit-map nil
  "The buffer's local keymap, while it's swapped for `dr/comment-map'.")

(defvar dr--quiet nil
  "Non-nil while the comments are taken out and put back, which aren't edits.")

;;
;; The review file.

(defun dr--store-file ()
  (expand-file-name "review.json" (magit-gitdir)))

(defun dr--read-store ()
  "The list of comments in the review file, as alists."
  (let ((file (dr--store-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (unless (string-blank-p (buffer-string))
          (json-parse-buffer :object-type 'alist :array-type 'list
                             :null-object nil :false-object nil))))))

(defun dr--write-store (comments)
  "Write COMMENTS to the review file, or delete the file when there are none."
  (let ((file (dr--store-file)))
    (if comments
        (let ((json-encoding-pretty-print t))
          (with-temp-file file
            (insert (json-encode (vconcat comments)) "\n")))
      (when (file-exists-p file)
        (delete-file file)))))

;;
;; The commit, and the lines of its diff.

(defun dr--patch-id (sha)
  "The stable patch-id of commit SHA, or nil, e.g. for an empty commit."
  (with-temp-buffer
    (magit-git-insert "show" "--format=" "--no-color" "--no-ext-diff" sha)
    (call-process-region (point-min) (point-max) (magit-git-executable) t t nil
                         "patch-id" "--stable")
    (car (split-string (buffer-string)))))

(defun dr--commit-info ()
  (let ((sha magit-buffer-revision-oid))
    (list :sha sha
          :subject (magit-rev-format "%s" sha)
          :patch-id (dr--patch-id sha))))

(defun dr--matches-commit-p (comment info)
  "Whether COMMENT is on the commit described by INFO, a plist from `dr--commit-info'."
  (let ((patch-id (plist-get info :patch-id)))
    (or (equal (alist-get 'commit comment) (plist-get info :sha))
        (and patch-id (equal (alist-get 'patch_id comment) patch-id))
        (equal (alist-get 'subject comment) (plist-get info :subject)))))

(defun dr--comment-id-at (pos)
  (and (< pos (point-max))
       (get-text-property pos 'dr-comment)))

(defun dr--next-bol (pos)
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (point)))

(defun dr--sections (type)
  "The buffer's sections of TYPE, e.g. `file', in buffer order. This skips the file sections of
the diffstat (shown with --stat), which are just a line each, with no hunks."
  (let (sections)
    (magit-map-sections (lambda (section)
                          (when (and (eq (oref section type) type)
                                     (not (dr--in-diffstat-p section)))
                            (push section sections))))
    (sort sections (lambda (a b) (< (oref a start) (oref b start))))))

(defun dr--hunk-lines (hunk)
  "The lines of HUNK, as plists (:bol :side :line :code), skipping comments. Nil for the hunks of
a merge commit, which have more than one parent's line numbers."
  (with-slots (content end combined from-range to-range) hunk
    (when (and (not combined) from-range to-range)
      (save-excursion
        (goto-char content)
        (let ((old (car from-range))
              (new (car to-range))
              lines)
          (while (< (point) end)
            (unless (dr--comment-id-at (point))
              (let* ((eol (line-end-position))
                     (code (buffer-substring-no-properties (min (1+ (point)) eol) eol)))
                (pcase (char-after)
                  (?- (push (list :bol (point) :side "old" :line old :code code) lines)
                      (cl-incf old))
                  (?+ (push (list :bol (point) :side "new" :line new :code code) lines)
                      (cl-incf new))
                  (?\s (push (list :bol (point) :side "new" :line new :code code) lines)
                       (cl-incf old)
                       (cl-incf new)))))
            (forward-line))
          (nreverse lines))))))

(defun dr--in-diffstat-p (section)
  (when-let* ((parent (oref section parent)))
    (eq (oref parent type) 'diffstat)))

(defun dr--file-section (file)
  (seq-find (lambda (section) (equal (oref section value) file))
            (dr--sections 'file)))

(defun dr--file-lines (file-section)
  "The lines of FILE-SECTION's hunks, as from `dr--hunk-lines', each with a :hunk."
  (mapcan (lambda (hunk)
            (mapcar (lambda (line) (append line (list :hunk hunk)))
                    (dr--hunk-lines hunk)))
          (seq-filter (lambda (s) (eq (oref s type) 'hunk))
                      (oref file-section children))))

(defun dr--commit-place ()
  "Where commit comments go, as (POS SECTION): after the commit message, before the blank line
which ends it."
  (if-let* ((message (car (dr--sections 'commit-message))))
      (let ((end (oref message end)))
        (save-excursion
          (goto-char end)
          (forward-line -1)
          (list (if (and (looking-at-p "^$") (> (point) (oref message start)))
                    (point)
                  end)
                message)))
    (let ((file (car (dr--sections 'file))))
      (list (if file (oref file start) (point-max))
            magit-root-section))))

(defun dr--file-place (file-section)
  "Where FILE-SECTION's comments go, as (POS SECTION): right below its heading."
  (list (oref file-section content) file-section))

(defun dr--line-place (comment)
  "Where line COMMENT goes, as (POS SECTION ORPHAN): right below its line, or with its file's or
commit's comments if its line can't be found. ORPHAN is non-nil in that case."
  (let-alist comment
    (let* ((file (dr--file-section .file))
           (candidates (and file
                            (seq-filter (lambda (l)
                                          (and (equal (plist-get l :side) .side)
                                               (equal (plist-get l :code) .code)))
                                        (dr--file-lines file))))
           (line (seq-reduce (lambda (best l)
                               (if (or (null best)
                                       (< (abs (- (plist-get l :line) .line))
                                          (abs (- (plist-get best :line) .line))))
                                   l
                                 best))
                             candidates nil)))
      (cond (line (list (dr--next-bol (plist-get line :bol)) (plist-get line :hunk) nil))
            (file (append (dr--file-place file) '(t)))
            (t (append (dr--commit-place) '(t)))))))

(defun dr--place (comment)
  "Where COMMENT goes, as (POS SECTION ORPHAN). See `dr--line-place'."
  (let-alist comment
    (pcase .kind
      ("line" (dr--line-place comment))
      ("file" (if-let* ((file (dr--file-section .file)))
                  (append (dr--file-place file) '(nil))
                (append (dr--commit-place) '(t))))
      (_ (append (dr--commit-place) '(nil))))))

;;
;; Comment text in the buffer.

(defun dr--props (id)
  (pcase-let ((`(,section . ,bar) (gethash id dr--placements)))
    (list 'dr-comment id
          'magit-section section
          'font-lock-face 'dr/comment
          'line-prefix bar
          'wrap-prefix bar)))

(defun dr--region (id)
  "The (BEG . END) of comment ID's text, including its final newline, or nil."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward 'dr-comment id t)))
      (cons (prop-match-beginning match) (prop-match-end match)))))

(defun dr--unstick (pos)
  "Let text be inserted at POS, the start of a comment, despite the read-only text before it."
  (when (> pos (point-min))
    (let ((nonsticky (get-text-property (1- pos) 'rear-nonsticky)))
      (unless (or (eq nonsticky t) (memq 'read-only nonsticky))
        (put-text-property (1- pos) pos 'rear-nonsticky (cons 'read-only nonsticky))))))

(defun dr--insert (id text pos section bar)
  "Insert TEXT as the text of comment ID at POS, in SECTION. TEXT ends with a newline."
  (puthash id (cons section bar) dr--placements)
  (save-excursion
    (goto-char pos)
    (insert (apply #'propertize text (dr--props id)))
    ;; Text inserted at the very end of a section lands outside of it, because a section's end
    ;; marker doesn't advance when text is inserted at it. That happens for a comment on the last
    ;; line of a hunk, which belongs inside the hunk.
    (let ((s section))
      (while s
        (when (= (oref s end) pos)
          (set-marker (oref s end) (point)))
        (setq s (oref s parent)))))
  (dr--unstick pos))

(defun dr--render ()
  "Insert the comments on this buffer's commit, and make all other text read-only."
  (let ((inhibit-read-only t)
        (dr--quiet t)
        (buffer-undo-list t)
        (placed nil)
        (i 0))
    (put-text-property (point-min) (point-max) 'read-only t)
    (setq dr--comments (make-hash-table :test #'equal))
    (setq dr--placements (make-hash-table :test #'equal))
    (dolist (comment (dr--read-store))
      (when (dr--matches-commit-p comment dr--commit)
        (puthash (alist-get 'id comment) comment dr--comments)
        (let ((place (dr--place comment)))
          ;; Orphans go after the comments which really belong at the same place.
          (push (cons (cl-incf i (if (nth 2 place) 100000 1))
                      (cons comment place))
                placed))))
    ;; Inserting from the end of the buffer backwards keeps the positions of the places before
    ;; each insertion valid. At the same position, the later comment is inserted first.
    (setq placed (sort placed (lambda (a b)
                                (let ((pos-a (nth 2 a))
                                      (pos-b (nth 2 b)))
                                  (or (> pos-a pos-b)
                                      (and (= pos-a pos-b) (> (car a) (car b))))))))
    (pcase-dolist (`(,_ ,comment ,pos ,section ,orphan) placed)
      (dr--insert (alist-get 'id comment) (concat (alist-get 'comment comment) "\n")
                  pos section (if orphan dr--orphan-bar dr--bar)))))

(defun dr--remove-all ()
  "Delete the text of every comment from the buffer."
  (let ((inhibit-read-only t)
        (dr--quiet t)
        (buffer-undo-list t)
        (pos (point-min)))
    (while (setq pos (text-property-not-all pos (point-max) 'dr-comment nil))
      (delete-region pos (or (next-single-property-change pos 'dr-comment) (point-max))))))

(defun dr--any-p (&optional beg end)
  (text-property-not-all (or beg (point-min)) (or end (point-max)) 'dr-comment nil))

;;
;; Editing.

(defun dr--ids-near (beg end)
  "The ids of the comments in BEG..END, or touching it."
  (let ((ids (list (dr--comment-id-at beg)
                   (and (> beg (point-min)) (dr--comment-id-at (1- beg)))
                   (dr--comment-id-at end)))
        (pos beg))
    (while (setq pos (dr--any-p pos end))
      (push (get-text-property pos 'dr-comment) ids)
      (setq pos (next-single-property-change pos 'dr-comment nil end)))
    (delete-dups (delq nil ids))))

(defun dr--before-change (beg end)
  (unless dr--quiet
    (setq dr--chars-tick (buffer-chars-modified-tick))
    ;; A deletion can remove all of a comment's text, after which the comment can't be found.
    (setq dr--changing (dr--ids-near beg end))))

(defun dr--after-change (beg end _len)
  ;; Text property changes, such as magit painting a hunk, don't change the text.
  (unless (or dr--quiet (eql dr--chars-tick (buffer-chars-modified-tick)))
    (let ((ids dr--changing)
          (inhibit-read-only t))
      (when (< beg end)
        ;; Inserted text belongs to the comment it was inserted into, or next to.
        (when-let* ((id (or (get-text-property beg 'dr-comment)
                            (and (> beg (point-min)) (dr--comment-id-at (1- beg)))
                            (dr--comment-id-at end))))
          (push id ids)
          ;; Inserted text can inherit the properties of the diff text before it.
          (set-text-properties beg end (dr--props id))))
      (setq ids (delete-dups ids))
      (setq dr--dirty (delete-dups (append ids dr--dirty)))
      (dolist (id ids)
        (when-let* ((region (dr--region id)))
          ;; A comment must end with a newline, so that the diff line after it stays on its own
          ;; line, e.g. after the comment's last newline is deleted with the delete key.
          (unless (eq (char-before (cdr region)) ?\n)
            (save-excursion
              (goto-char (cdr region))
              (insert (apply #'propertize "\n" (dr--props id)))))
          (let ((buffer-undo-list t))
            (dr--unstick (car region))))))))

(defun dr--save ()
  "Write the comments which have changed to the review file. A comment left blank is deleted."
  (when dr--dirty
    (let ((store (dr--read-store)))
      (dolist (id dr--dirty)
        (let* ((region (dr--region id))
               (text (and region (string-remove-suffix
                                  "\n" (buffer-substring-no-properties (car region)
                                                                       (cdr region)))))
               (comment (gethash id dr--comments))
               (index (seq-position store id (lambda (c id) (equal (alist-get 'id c) id)))))
          (if (or (null text) (string-blank-p text))
              (progn
                (when region
                  ;; This isn't an edit to undo. Undoing it would only bring back the blank
                  ;; comment, which would be deleted again. Since it's left out of the undo
                  ;; history, the positions in that history are now wrong, so clear it.
                  (let ((inhibit-read-only t)
                        (dr--quiet t)
                        (was-in (<= (car region) (point) (1- (cdr region)))))
                    (delete-region (car region) (cdr region))
                    ;; Back to the line the comment was on, as when cancelling an `o' in vim.
                    (when was-in
                      (forward-line -1)))
                  (setq buffer-undo-list nil))
                (when index
                  (setq store (append (seq-take store index) (seq-drop store (1+ index))))))
            (setf (alist-get 'comment comment) text)
            (if index
                (setf (nth index store) comment)
              (setq store (append store (list comment)))))))
      (dr--write-store store)
      (setq dr--dirty nil))))

(defun dr--new-id ()
  (substring (md5 (format "%s %s" (float-time) (random))) 0 8))

(defun dr--anchor-at-point ()
  "What a comment opened at point is about, as a plist: the comment's fields (:kind, and for a line
comment :file, :side, :line, :code), and where it goes (:pos :section)."
  (let* ((section (magit-current-section))
         (file (if (eq (oref section type) 'hunk) (oref section parent) section)))
    ;; On a file's line in the diffstat, comment on the file itself.
    (when (and (eq (oref file type) 'file) (dr--in-diffstat-p file))
      (setq file (dr--file-section (oref file value)))
      (unless file
        (user-error "This file's diff isn't in this buffer")))
    (cond
     ((eq (oref section type) 'hunk)
      (when (< (point) (oref section content))
        (user-error "Comments go on the lines of a hunk, not its heading"))
      (let ((line (seq-find (lambda (l) (= (plist-get l :bol) (line-beginning-position)))
                            (dr--hunk-lines section))))
        (unless line
          (user-error (if (oref section combined)
                          "Comments on merge commits aren't supported"
                        "There's no line to comment on here")))
        (list :kind "line" :file (oref file value) :side (plist-get line :side)
              :line (plist-get line :line) :code (plist-get line :code)
              :pos (dr--next-bol (plist-get line :bol)) :section section)))
     ((eq (oref file type) 'file)
      (pcase-let ((`(,pos ,section) (dr--file-place file)))
        (list :kind "file" :file (oref file value) :pos pos :section section)))
     (t
      (pcase-let ((`(,pos ,section) (dr--commit-place)))
        (list :kind "commit" :pos pos :section section))))))

(defun dr--same-anchor-p (comment anchor)
  (let-alist comment
    (and (equal .kind (plist-get anchor :kind))
         (equal .file (plist-get anchor :file))
         (equal .side (plist-get anchor :side))
         (equal .line (plist-get anchor :line)))))

(defun dr/open-comment ()
  "Open a comment below the line at point, and start typing it. On a line of a hunk, the comment is
about that line. On a file's heading, it's about the whole file, and in the commit's headers or
message, the whole commit. If there's already a comment there, open a new line at its top."
  (interactive)
  (unless (derived-mode-p 'magit-revision-mode)
    (user-error "Comments go in a revision buffer"))
  (let* ((anchor (dr--anchor-at-point))
         (pos (plist-get anchor :pos))
         (existing (dr--comment-id-at pos))
         (inhibit-read-only t))
    (if (and existing (dr--same-anchor-p (gethash existing dr--comments) anchor))
        (progn
          (goto-char pos)
          (insert "\n")
          (goto-char pos))
      (let* ((id (dr--new-id))
             (info dr--commit)
             (comment `((id . ,id)
                        (kind . ,(plist-get anchor :kind))
                        (commit . ,(plist-get info :sha))
                        (subject . ,(plist-get info :subject))
                        (patch_id . ,(plist-get info :patch-id))
                        (file . ,(plist-get anchor :file))
                        (side . ,(plist-get anchor :side))
                        (line . ,(plist-get anchor :line))
                        (code . ,(plist-get anchor :code))
                        (comment . ""))))
        (puthash id comment dr--comments)
        (let ((dr--quiet t))
          (dr--insert id "\n" pos (plist-get anchor :section) dr--bar))
        (push id dr--dirty)
        (goto-char pos))))
  (dr--update-in-comment)
  (evil-insert-state))

;;
;; Hooks and advice.

(defun dr--update-in-comment ()
  "Make the buffer editable, with `dr/comment-map' as its keymap, while point is in a comment."
  (let ((in-comment (and (dr--comment-id-at (point)) t)))
    (setq buffer-read-only (not in-comment))
    (unless (eq in-comment dr--in-comment)
      (setq dr--in-comment in-comment)
      (if in-comment
          (progn
            (setq dr--magit-map (current-local-map))
            (use-local-map dr/comment-map)
            (setq auto-fill-function #'do-auto-fill))
        (use-local-map dr--magit-map)
        (setq auto-fill-function nil))
      (evil-normalize-keymaps))))

(defun dr--pre-command ()
  ;; Undoing from a diff line can restore a comment deleted with dd.
  (when (memq this-command dr--undo-commands)
    (setq buffer-read-only nil)))

(defun dr--post-command ()
  (unless (memq evil-state '(insert replace))
    (dr--save))
  (dr--update-in-comment))

(defun dr--point-spot ()
  "Where point is, as (ID . OFFSET) if it's in comment ID, or else a marker which stays after
comments inserted at it."
  (if-let* ((id (dr--comment-id-at (point))))
      (cons id (- (point) (car (dr--region id))))
    (copy-marker (point) t)))

(defun dr--goto-spot (spot)
  (if (markerp spot)
      (progn (goto-char spot)
             (set-marker spot nil))
    (when-let* ((region (dr--region (car spot))))
      (goto-char (min (+ (car region) (cdr spot)) (1- (cdr region)))))))

(defun dr--around-refresh (refresh &rest args)
  "Take the comments out before magit refreshes the buffer, so that the positions it saves and
restores are of diff text only, and then put them back in."
  (if (not (derived-mode-p 'magit-revision-mode))
      (apply refresh args)
    (dr--save)
    (let ((spot (dr--point-spot))
          (buffer-undo-list t)
          (dr--quiet t))
      (dr--remove-all)
      (apply refresh args)
      (setq dr--commit (dr--commit-info))
      ;; Magit has restored point to where it was in the diff, so keep it after any comment
      ;; inserted there, unless it was in a comment.
      (let ((after-comments (copy-marker (point) t)))
        (dr--render)
        (dr--goto-spot (if (markerp spot) after-comments spot)))
      (when (markerp spot)
        (set-marker spot nil)))
    (setq buffer-undo-list nil)
    (dr--update-in-comment)))

(defun dr--around-visit (visit &rest args)
  "Visit with the comments taken out of the buffer, since magit counts the lines of a hunk to find
the line to visit."
  (if (not (and (derived-mode-p 'magit-revision-mode) (dr--any-p)))
      (apply visit args)
    (dr--save)
    (let ((spot (dr--point-spot))
          (buffer (current-buffer))
          (dr--quiet t))
      (dr--remove-all)
      (unwind-protect
          (apply visit args)
        ;; The same comments go back in at the same places, so the undo history stays valid.
        (with-current-buffer buffer
          (dr--render)
          (dr--goto-spot spot))))))

(defun dr--before-apply (sections &rest _)
  "Refuse to apply or reverse hunks which have comments, since magit makes the patch from the text
of the hunks."
  (when (derived-mode-p 'magit-revision-mode)
    (dolist (section (ensure-list sections))
      (when (dr--any-p (oref section start) (oref section end))
        (user-error "Can't apply or reverse a hunk with review comments in it")))))

(defun dr--before-apply-command (&rest _)
  "Refuse to apply, reverse, or discard from a revision buffer when the selected hunks, or the hunk
or file at point, have comments. Checking before the command runs means there's no confirmation
prompt first."
  (when (derived-mode-p 'magit-revision-mode)
    (dr--before-apply (or (magit-region-sections '(hunk file)) (magit-current-section)))))

(defun dr--around-paint (paint section &rest args)
  "Magit paints the lines of a hunk when point moves in or out of it. Keep it from painting the
comments, and from recording the paint in the undo history, which holds only comment edits."
  (if (not (derived-mode-p 'magit-revision-mode))
      (apply paint section args)
    (let ((buffer-undo-list t)
          (inhibit-read-only t)
          (dr--quiet t)
          (end (oref section end)))
      (apply paint section args)
      (let ((pos (oref section start)))
        (while (setq pos (dr--any-p pos end))
          (let ((comment-end (or (next-single-property-change pos 'dr-comment nil end) end)))
            (put-text-property pos comment-end 'font-lock-face 'dr/comment)
            (remove-text-properties pos comment-end '(face nil))
            (setq pos comment-end)))))))

(defun dr--setup ()
  ;; Auto-fill, which my init.el turns on, works in comments. These keep it from treating the diff
  ;; lines around a comment as part of the comment's paragraph.
  (setq-local adaptive-fill-mode nil)
  (setq-local comment-line-break-function (lambda (&optional _) (insert "\n")))
  (setq-local buffer-undo-list nil)
  (add-hook 'before-change-functions #'dr--before-change nil t)
  (add-hook 'after-change-functions #'dr--after-change nil t)
  (add-hook 'pre-command-hook #'dr--pre-command nil t)
  (add-hook 'post-command-hook #'dr--post-command nil t)
  (add-hook 'kill-buffer-hook #'dr--save nil t))

(add-hook 'magit-revision-mode-hook #'dr--setup)
(advice-add 'magit-refresh-buffer :around #'dr--around-refresh)
(advice-add 'magit-diff-visit-file--noselect :around #'dr--around-visit)
(advice-add 'magit-apply-patch :before #'dr--before-apply)
(dolist (command '(magit-apply magit-reverse magit-discard))
  (advice-add command :before #'dr--before-apply-command))
(advice-add 'magit-section-paint :around #'dr--around-paint)
