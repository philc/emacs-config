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
;; Threads: Claude replies to a comment in review.json (see the /cr skill), and I reply to Claude,
;; as in a thread on a GitHub PR.
;; - A thread shows below its comment. Claude's replies are read-only, and commit hashes in them are
;;   links (RET), e.g. to the fixup commit which addressed the comment. A link opens in another
;;   window, keeping the focus on the thread, like `o' on a commit in the log.
;; - `o' anywhere on a thread starts a reply at its end. Replying reopens a resolved thread.
;; - Claude marks a thread resolved when there's nothing left for me to answer, and R on a thread
;;   resolves or reopens it. A resolved thread is marked with a checkmark, and stays unfolded, since
;;   I read Claude's replies to see what it did.
;; - gn and gp go to the next and previous comment, rather than hunk, when there are any. Reading
;;   the comments is what these buffers are for.
;; - X on a thread deletes the comment and its thread.
;; - Saving a comment merges it into the file's latest version of it, so that saving doesn't
;;   overwrite replies Claude has added since the buffer was refreshed.
;;
;; The review buffer (`dr/show-review') shows every comment on the branch, with its thread, in one
;; place, like the conversation tab of a GitHub PR, so that after Claude replies I don't have to
;; open each commit to read the replies. Comments are in the order they appear in the branch's
;; diffs: oldest commit first, then by file, then by line. Each line comment shows the few lines
;; of the diff above it. The keys work as in a revision buffer, and RET opens the commit at the
;; comment. The review buffer's mode derives from magit-revision-mode, so all of the machinery for
;; comments in revision buffers applies to it; what differs is where each comment's text goes.
;;
;; A comment in .git/review.json looks like:
;;   {"id": "3f2a9c1e", "kind": "line", "commit": "<hash>", "subject": "link-hints: add ...",
;;    "patch_id": "<patch-id>", "file": "content_scripts/link_hints.js", "side": "new",
;;    "line": 142, "code": "    const text = el.innerText.trim();", "comment": "Use textContent...",
;;    "thread": [{"author": "claude", "text": "Done in a1b2c3d."},
;;               {"id": "9e8d7c6b", "author": "you", "text": "Thanks. Also..."}],
;;    "resolved": true}
;; "kind" is "line", "file", or "commit". "side" is "new" for an added or context line, whose "line"
;; is in the commit's version of the file, and "old" for a removed line, whose "line" is in the
;; parent's version. "thread" and "resolved" are optional. My replies have ids, so that each can be
;; saved on its own.
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

(defface dr/reply
  '((((background dark)) :background "#2e2b3a" :extend t)
    (t :background "#f4f0f9" :extend t))
  "Face for Claude's replies in a comment's thread."
  :group 'magit-faces)

(defface dr/reply-header
  '((t :inherit dr/comment-bar :weight bold))
  "Face for the line naming who wrote each reply in a thread."
  :group 'magit-faces)

(defface dr/link
  '((t :inherit link))
  "Face for commit hashes in replies, which RET shows."
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

(defvar-local dr--review-places nil
  "In the review buffer, where each comment goes, as a list of (COMMENT POS SECTION ORPHAN), made
when the buffer is refreshed.")

(defvar-local dr--review-infos nil
  "In the review buffer, a hash table from each commit's hash to its plist from `dr--commit-info'.")

(defvar-local dr--comments nil
  "A hash table from the id of each comment shown in this buffer to the comment's alist.")

(defvar-local dr--placements nil
  "A hash table from the id of each comment shown in this buffer to (SECTION . BAR): the magit
section its text belongs to, and the bar to display before its lines.")

(defvar-local dr--units nil
  "A hash table from the id of each piece of text I can edit, to (COMMENT-ID . REPLY-ID). The text
of a comment itself has the comment's id, and REPLY-ID nil. Each of my replies has its own id.")

(defvar-local dr--dirty nil
  "The ids of the units (see `dr--units') whose text has changed since they were last saved.")

(defvar-local dr--changing nil
  "The ids of the units a change in progress may affect. See `dr--before-change'.")

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

(defun dr--json-ready (comment)
  "COMMENT with its thread as a vector, which `json-encode' writes as an array rather than guessing."
  (mapcar (lambda (pair)
            (if (eq (car pair) 'thread)
                (cons 'thread (vconcat (cdr pair)))
              pair))
          comment))

(defun dr--write-store (comments)
  "Write COMMENTS to the review file, or delete the file when there are none."
  (let ((file (dr--store-file)))
    (if comments
        (let ((json-encoding-pretty-print t))
          (with-temp-file file
            (insert (json-encode (vconcat (mapcar #'dr--json-ready comments))) "\n")))
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

(defun dr--commit-info (&optional sha)
  (let ((sha (or sha magit-buffer-revision-oid)))
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
            (unless (get-text-property (point) 'dr-block)
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

(defun dr--props (unit)
  "The text properties of editable UNIT, the text of a comment or of one of my replies."
  (let ((cid (car (gethash unit dr--units))))
    (pcase-let ((`(,section . ,bar) (gethash cid dr--placements)))
      (list 'dr-comment unit
            'dr-block cid
            'magit-section section
            'font-lock-face 'dr/comment
            'dr-face 'dr/comment
            'line-prefix bar
            'wrap-prefix bar))))

(defvar dr--thread-map
  (let ((map (make-sparse-keymap))
        (show `(menu-item "" dr/show-commit
                          :filter ,(lambda (cmd) (and (get-text-property (point) 'dr-rev) cmd)))))
    ;; Off a hash, RET keeps its meaning, since a filter which returns nil leaves the key unbound in
    ;; this map.
    (dolist (key '("RET" "<return>"))
      (define-key map (kbd key) show))
    map)
  "The keymap of the read-only text of a thread. As a `keymap' text property, it takes precedence
over evil's keymaps.")

(defun dr--fixed (cid text face &rest props)
  "TEXT as read-only text in the block of comment CID, e.g. one of Claude's replies, in FACE."
  (pcase-let ((`(,section . ,bar) (gethash cid dr--placements)))
    (apply #'propertize text
           'read-only t
           'dr-block cid
           'magit-section section
           'font-lock-face face
           'dr-face face
           'line-prefix bar
           'wrap-prefix bar
           'keymap dr--thread-map
           props)))

(defun dr--linkify (text)
  "TEXT with the commit hashes in it made into links."
  (let ((case-fold-search nil)
        (start 0))
    (while (string-match "\\b[0-9a-f]\\{7,40\\}\\b" text start)
      (let ((rev (match-string 0 text))
            (beg (match-beginning 0)))
        (setq start (match-end 0))
        (when (magit-commit-p rev)
          (add-text-properties beg start (list 'dr-rev rev
                                               'font-lock-face '(dr/link dr/reply)
                                               'dr-face '(dr/link dr/reply))
                               text))))
    text))

(defun dr--wrap (text)
  "TEXT with each line too long for `fill-column', after the bar, broken at spaces. A continuation
is indented to line up with the line's text, after any indentation or list marker. Lines in
fenced code blocks are left alone. This is for Claude's replies, whose paragraphs are one line."
  (let ((width (- (default-value 'fill-column) (string-width dr--bar)))
        (in-code nil))
    (mapconcat
     (lambda (line)
       (when (string-prefix-p "```" (string-trim-left line))
         (setq in-code (not in-code)))
       (if (or in-code (<= (string-width line) width))
           line
         (string-match "\\`[ \t]*\\(?:[-*] \\|[0-9]+[.)] \\)?" line)
         (with-temp-buffer
           (insert line)
           (let ((fill-column width)
                 (fill-prefix (make-string (match-end 0) ?\s))
                 (adaptive-fill-mode nil))
             (fill-region-as-paragraph (point-min) (point-max)))
           (string-trim-right (buffer-string)))))
     (split-string text "\n")
     "\n")))

(defun dr--block-text (comment)
  "The text of COMMENT's block: the comment, then its thread."
  (let-alist comment
    (let ((cid .id))
      (puthash cid (cons cid nil) dr--units)
      (apply #'concat
             (apply #'propertize (concat .comment "\n") (dr--props cid))
             (mapcar (lambda (entry) (dr--entry-text cid entry)) .thread)))))

(defun dr--entry-text (cid entry)
  "The text of ENTRY, a reply in the thread of comment CID: a line naming its author, then the
reply. Mine are editable, and Claude's aren't."
  (let-alist entry
    (let ((mine (equal .author "you")))
      (concat (dr--fixed cid (if mine "── You\n" "── Claude\n") '(dr/reply-header dr/comment)
                         'dr-header (or .id t))
              (if (and mine .id)
                  (progn
                    (puthash .id (cons cid .id) dr--units)
                    (apply #'propertize (concat .text "\n") (dr--props .id)))
                (dr--linkify (dr--fixed cid (concat (if mine .text (dr--wrap .text)) "\n")
                                        (if mine 'dr/comment 'dr/reply))))))))

(defun dr--region (unit)
  "The (BEG . END) of editable UNIT's text, including its final newline, or nil."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward 'dr-comment unit t)))
      (cons (prop-match-beginning match) (prop-match-end match)))))

(defun dr--block-region (cid)
  "The (BEG . END) of all of comment CID's text, its thread included, or nil."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward 'dr-block cid t)))
      (cons (prop-match-beginning match) (prop-match-end match)))))

(defun dr--unstick (pos)
  "Let text be inserted at POS, the start of a comment, despite the read-only text before it."
  (when (> pos (point-min))
    (let ((nonsticky (get-text-property (1- pos) 'rear-nonsticky)))
      (unless (or (eq nonsticky t) (memq 'read-only nonsticky))
        (put-text-property (1- pos) pos 'rear-nonsticky (cons 'read-only nonsticky))))))

(defun dr--insert (text pos section)
  "Insert TEXT, the text of a comment's block or part of one, at POS in SECTION. TEXT ends with a
newline."
  (save-excursion
    (goto-char pos)
    (insert text)
    ;; Text inserted at the very end of a section lands outside of it, because a section's end
    ;; marker doesn't advance when text is inserted at it. That happens for a comment on the last
    ;; line of a hunk, which belongs inside the hunk.
    (let ((s section))
      (while s
        (when (= (oref s end) pos)
          (set-marker (oref s end) (point)))
        (setq s (oref s parent))))
    (let ((unit-pos pos))
      (while (setq unit-pos (text-property-not-all unit-pos (point) 'dr-comment nil))
        (dr--unstick unit-pos)
        (setq unit-pos (next-single-property-change unit-pos 'dr-comment nil (point)))))))

(defun dr--places ()
  "The comments this buffer shows, and where each goes, as a list of (COMMENT POS SECTION ORPHAN)."
  (if (derived-mode-p 'dr/review-mode)
      dr--review-places
    (mapcar (lambda (comment) (cons comment (dr--place comment)))
            (seq-filter (lambda (comment) (dr--matches-commit-p comment dr--commit))
                        (dr--read-store)))))

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
    (setq dr--units (make-hash-table :test #'equal))
    (pcase-dolist (`(,comment . ,place) (dr--places))
      (puthash (alist-get 'id comment) comment dr--comments)
      ;; Orphans go after the comments which really belong at the same place.
      (push (cons (cl-incf i (if (nth 2 place) 100000 1))
                  (cons comment place))
            placed))
    ;; Inserting from the end of the buffer backwards keeps the positions of the places before
    ;; each insertion valid. At the same position, the later comment is inserted first.
    (setq placed (sort placed (lambda (a b)
                                (let ((pos-a (nth 2 a))
                                      (pos-b (nth 2 b)))
                                  (or (> pos-a pos-b)
                                      (and (= pos-a pos-b) (> (car a) (car b))))))))
    (pcase-dolist (`(,_ ,comment ,pos ,section ,orphan) placed)
      (puthash (alist-get 'id comment) (cons section (if orphan dr--orphan-bar dr--bar))
               dr--placements)
      (dr--insert (dr--block-text comment) pos section)
      (when (alist-get 'resolved comment)
        (dr--mark-resolved pos)))))

(defconst dr--resolved-mark (propertize "✓ " 'face 'dr/comment-bar)
  "Displayed before the first line of a resolved comment.")

(defun dr--mark-resolved (pos)
  "Show a checkmark before the line at POS, the first of a resolved comment's block. It's an
overlay, rather than text, so that it isn't part of the comment. It's deleted along with the
block's text."
  (let ((overlay (make-overlay pos (dr--next-bol pos) nil t nil)))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'before-string dr--resolved-mark)))

(defun dr--remove-all ()
  "Delete the text of every comment from the buffer."
  (let ((inhibit-read-only t)
        (dr--quiet t)
        (buffer-undo-list t)
        (pos (point-min)))
    (while (setq pos (text-property-not-all pos (point-max) 'dr-block nil))
      (delete-region pos (or (next-single-property-change pos 'dr-block) (point-max))))))

(defun dr--any-p (&optional beg end)
  "Whether there's any comment text, editable or not, in BEG..END."
  (text-property-not-all (or beg (point-min)) (or end (point-max)) 'dr-block nil))

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

(defun dr--delete-unit (region reply-id)
  "Delete the text in REGION, of a blank comment, or of my blank reply REPLY-ID with the line
naming its author. This isn't an edit to undo: undoing it would only bring back the blank text,
which would be deleted again. Since it's left out of the undo history, the positions in that
history are now wrong, so clear it."
  (let ((inhibit-read-only t)
        (dr--quiet t)
        (was-in (<= (car region) (point) (1- (cdr region))))
        (beg (car region)))
    (when reply-id
      (save-excursion
        (goto-char beg)
        (forward-line -1)
        (when (equal (get-text-property (point) 'dr-header) reply-id)
          (setq beg (point)))))
    (delete-region beg (cdr region))
    ;; Back to the line before, as when cancelling an `o' in vim.
    (when was-in
      (goto-char beg)
      (forward-line -1)))
  (setq buffer-undo-list nil))

(defun dr--set-reply (comment reply-id text)
  "COMMENT with my reply REPLY-ID set to TEXT, or removed when TEXT is nil. Replying reopens a
resolved thread."
  (let* ((thread (copy-tree (alist-get 'thread comment)))
         (entry (seq-find (lambda (e) (equal (alist-get 'id e) reply-id)) thread)))
    (cond ((null text) (setq thread (delq entry thread)))
          (entry (setf (alist-get 'text entry) text))
          (t (setq thread (append thread `(((id . ,reply-id) (author . "you") (text . ,text)))))))
    (setf (alist-get 'thread comment nil t) thread)
    (when text
      (setf (alist-get 'resolved comment nil t) nil))
    comment))

(defun dr--save ()
  "Write the comments and replies which have changed to the review file. A blank reply is deleted,
and so is a blank comment, unless it has a thread."
  (when dr--dirty
    (let ((store (dr--read-store)))
      (dolist (unit dr--dirty)
        (pcase-let* ((`(,cid . ,reply-id) (gethash unit dr--units))
                     (region (dr--region unit))
                     (text (and region (string-remove-suffix
                                        "\n" (buffer-substring-no-properties (car region)
                                                                             (cdr region)))))
                     (blank (or (null text) (string-blank-p text)))
                     (index (seq-position store cid (lambda (c id) (equal (alist-get 'id c) id))))
                     ;; The file's version may have replies the buffer doesn't show yet.
                     (comment (copy-alist (if index (nth index store) (gethash cid dr--comments))))
                     (keep (or reply-id (not blank) (alist-get 'thread comment))))
          (when (and blank region (or reply-id (not keep)))
            (dr--delete-unit region reply-id))
          (when keep
            (setq comment (if reply-id
                              (dr--set-reply comment reply-id (unless blank text))
                            (setf (alist-get 'comment comment) (if blank "" text))
                            comment))
            ;; When a comment is deleted, this keeps it, so that undo can bring it back.
            (puthash cid comment dr--comments))
          (setq store (cond ((and keep index) (progn (setf (nth index store) comment) store))
                            (index (append (seq-take store index) (seq-drop store (1+ index))))
                            (keep (append store (list comment)))
                            (t store)))))
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
     ((derived-mode-p 'dr/review-mode)
      (unless (eq (oref section type) 'commit)
        (user-error "Comments go on a commit, file, or line"))
      (list :kind "commit" :pos (oref section content) :section section))
     (t
      (pcase-let ((`(,pos ,section) (dr--commit-place)))
        (list :kind "commit" :pos pos :section section))))))

(defun dr--commit-at-point ()
  "The plist from `dr--commit-info' of the commit a comment opened at point would be on."
  (if (derived-mode-p 'dr/review-mode)
      (let ((section (magit-current-section)))
        (while (and section (not (eq (oref section type) 'commit)))
          (setq section (oref section parent)))
        (unless section
          (user-error "Not on a commit"))
        (gethash (oref section value) dr--review-infos))
    dr--commit))

(defun dr--same-anchor-p (comment anchor)
  (let-alist comment
    (and (equal .kind (plist-get anchor :kind))
         (equal .file (plist-get anchor :file))
         (equal .side (plist-get anchor :side))
         (equal .line (plist-get anchor :line)))))

(defun dr--reply (cid)
  "Start typing a reply at the end of comment CID's thread."
  (let ((comment (gethash cid dr--comments)))
    (let* ((last (car (last (alist-get 'thread comment))))
           (last-id (and (equal (alist-get 'author last) "you") (alist-get 'id last)))
           (inhibit-read-only t))
      (if last-id
          ;; My reply is already the last word, so add a line to it.
          (progn
            (goto-char (1- (cdr (dr--region last-id))))
            (insert "\n"))
        (let ((reply-id (dr--new-id))
              (end (cdr (dr--block-region cid))))
          (puthash reply-id (cons cid reply-id) dr--units)
          (let ((dr--quiet t))
            (dr--insert (concat (dr--fixed cid "── You\n" '(dr/reply-header dr/comment)
                                           'dr-header reply-id)
                                (apply #'propertize "\n" (dr--props reply-id)))
                        end (car (gethash cid dr--placements))))
          (push reply-id dr--dirty)
          (goto-char (car (dr--region reply-id))))))))

(defun dr/open-comment ()
  "Open a comment below the line at point, and start typing it. On a line of a hunk, the comment is
about that line. On a file's heading, it's about the whole file, and in the commit's headers or
message, the whole commit. If there's already a comment there, open a new line at its top, or if
it has a thread, reply to it. On a thread, reply to it."
  (interactive)
  (unless (derived-mode-p 'magit-revision-mode)
    (user-error "Comments go in a revision buffer"))
  (if-let* ((cid (get-text-property (point) 'dr-block)))
      (dr--reply cid)
    (let* ((anchor (dr--anchor-at-point))
           (pos (plist-get anchor :pos))
           (existing (and (< pos (point-max)) (get-text-property pos 'dr-block)))
           (existing-comment (and existing (gethash existing dr--comments)))
           (inhibit-read-only t))
      (cond
       ((not (and existing-comment (dr--same-anchor-p existing-comment anchor)))
        (let* ((id (dr--new-id))
               (info (dr--commit-at-point))
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
          (puthash id (cons id nil) dr--units)
          (puthash id (cons (plist-get anchor :section) dr--bar) dr--placements)
          (let ((dr--quiet t))
            (dr--insert (apply #'propertize "\n" (dr--props id)) pos (plist-get anchor :section)))
          (push id dr--dirty)
          (goto-char pos)))
       ((or (alist-get 'thread existing-comment) (alist-get 'resolved existing-comment))
        (dr--reply existing))
       (t
        (goto-char pos)
        (insert "\n")
        (goto-char pos)))))
  (dr--update-in-comment)
  (evil-insert-state))

(defun dr/toggle-resolved ()
  "Mark the thread at point resolved, or reopen it."
  (interactive)
  (let ((cid (get-text-property (point) 'dr-block)))
    (unless cid
      (user-error "Not on a comment or its thread"))
    (dr--save)
    (let* ((store (dr--read-store))
           (index (seq-position store cid (lambda (c id) (equal (alist-get 'id c) id)))))
      (unless index
        (user-error "This comment hasn't been saved"))
      (let* ((comment (copy-alist (nth index store)))
             (resolved (not (alist-get 'resolved comment))))
        (setf (alist-get 'resolved comment nil t) resolved)
        (setf (nth index store) comment)
        (dr--write-store store)
        (puthash cid comment dr--comments)
        (dr--rerender)
        (message (if resolved "Resolved" "Reopened"))))))

(defun dr/delete-thread ()
  "Delete the comment at point, with its thread, after asking."
  (interactive)
  (let* ((cid (get-text-property (point) 'dr-block))
         (region (and cid (dr--block-region cid))))
    (unless cid
      (user-error "Not on a comment or its thread"))
    (when (y-or-n-p "Delete this comment and its thread? ")
      (dr--save)
      (dr--write-store (seq-remove (lambda (c) (equal (alist-get 'id c) cid)) (dr--read-store)))
      (let ((line (save-excursion (goto-char (car region)) (forward-line -1) (point-marker))))
        (dr--rerender)
        (goto-char line)
        (set-marker line nil))
      (dr--update-in-comment))))

(defun dr--block-starts ()
  "The positions where comments' blocks start, leaving out those in folded sections."
  (let ((pos (point-min))
        starts)
    (while (setq pos (dr--any-p pos))
      (unless (invisible-p pos)
        (push pos starts))
      (setq pos (next-single-property-change pos 'dr-block nil (point-max))))
    (nreverse starts)))

(defun dr/any-comments-p ()
  (and (derived-mode-p 'magit-revision-mode) (dr--any-p) t))

(defun dr--here ()
  "Point, or if it's in a comment's block, the block's start."
  (if-let* ((cid (get-text-property (point) 'dr-block)))
      (car (dr--block-region cid))
    (point)))

(defun dr/next-comment ()
  "Go to the next comment, skipping those in folded sections."
  (interactive)
  (if-let* ((pos (seq-find (lambda (p) (> p (dr--here))) (dr--block-starts))))
      (goto-char pos)
    (message "No more comments")))

(defun dr/previous-comment ()
  "Go to the start of this comment, or the previous one. See `dr/next-comment'."
  (interactive)
  (if-let* ((pos (car (last (seq-filter (lambda (p) (< p (point))) (dr--block-starts))))))
      (goto-char pos)
    (message "No more comments")))

(defun dr/show-commit ()
  "Show the commit whose hash is at point, in a reply, in another window, keeping the focus here.
From a revision buffer, the commit gets a revision buffer of its own, rather than replacing the
commit this one shows."
  (interactive)
  (let ((magit-display-buffer-noselect t)
        (magit-display-buffer-function
         (lambda (buffer)
           (display-buffer buffer '((display-buffer-reuse-window
                                     display-buffer-use-some-window
                                     display-buffer-pop-up-window)
                                    (inhibit-same-window . t)))))
        (magit-revision-use-dedicated-buffers (not (derived-mode-p 'dr/review-mode))))
    (magit-revision-setup-buffer (get-text-property (point) 'dr-rev)
                                 (car (magit-show-commit--arguments)) nil)))

(defun dr--rerender (&optional cid)
  "Take the comments out and put them back, e.g. after a thread is resolved. With CID, leave point
at the start of that comment's block."
  (dr--save)
  (let ((spot (if cid (list :block cid 0) (dr--point-spot)))
        (dr--quiet t))
    (when (derived-mode-p 'dr/review-mode)
      ;; The review buffer's places are made by its refresh, which this doesn't redo. Each
      ;; comment's place is a position in the text without comments, so use its latest version.
      (setq dr--review-places
            (let ((store (dr--read-store)))
              (delq nil (mapcar (pcase-lambda (`(,comment . ,place))
                                  (when-let* ((latest (seq-find (lambda (c)
                                                                  (equal (alist-get 'id c)
                                                                         (alist-get 'id comment)))
                                                                store)))
                                    (cons latest place)))
                                dr--review-places)))))
    (dr--remove-all)
    (dr--render)
    (dr--goto-spot spot))
  (setq buffer-undo-list nil)
  (dr--update-in-comment))

;;
;; The review buffer.

(define-derived-mode dr/review-mode magit-revision-mode "Review"
  "Every review comment on the branch, with its thread, in the order of the branch's diffs.")

(defconst dr--excerpt-lines 4
  "How many lines of the diff a line comment's excerpt shows, ending with the commented line.")

(defun dr/show-review ()
  "Show every review comment on the branch, with its thread."
  (interactive)
  (let ((name (format "magit-review: %s" (file-name-nondirectory
                                          (directory-file-name (magit-toplevel))))))
    (magit-setup-buffer #'dr/review-mode nil :buffer name)))

(defun dr--review-commits ()
  "The commits a comment may be on, oldest first: the branch's, or if HEAD is on the main branch,
its recent history."
  (let* ((main (magit-main-branch))
         (base (and main (magit-git-string "merge-base" "HEAD" main))))
    (if (and base (not (equal base (magit-rev-parse "HEAD"))))
        (magit-git-lines "rev-list" "--reverse" (concat base "..HEAD"))
      (magit-git-lines "rev-list" "--reverse" "-n" "100" "HEAD"))))

(defun dr--diff-lines (sha file)
  "The lines of FILE's diff in commit SHA, as plists (:side :line :old :new :text :hunk), where :old
and :new are the line numbers on each side, and :hunk counts the hunks."
  (let ((old 0) (new 0) (hunk 0) lines)
    (dolist (text (magit-git-lines "show" "--format=" "--no-color" "--no-ext-diff" "-U3"
                                   sha "--" file))
      (cond
       ((string-match "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)" text)
        (setq old (string-to-number (match-string 1 text))
              new (string-to-number (match-string 2 text)))
        (cl-incf hunk))
       ((zerop hunk))
       ((string-prefix-p "-" text)
        (push (list :side "old" :line old :old old :new new :text text :hunk hunk) lines)
        (cl-incf old))
       ((string-prefix-p "+" text)
        (push (list :side "new" :line new :old old :new new :text text :hunk hunk) lines)
        (cl-incf new))
       ((string-prefix-p " " text)
        (push (list :side "new" :line new :old old :new new :text text :hunk hunk) lines)
        (cl-incf old)
        (cl-incf new))))
    (nreverse lines)))

(defun dr--excerpt (lines comment)
  "The lines of LINES, from `dr--diff-lines', to show above line COMMENT, or nil if its line isn't
there."
  (let-alist comment
    (let ((target (seq-reduce (lambda (best l)
                                (if (and (equal (plist-get l :side) .side)
                                         (equal (substring (plist-get l :text) 1) .code)
                                         (or (null best)
                                             (< (abs (- (plist-get l :line) .line))
                                                (abs (- (plist-get best :line) .line)))))
                                    l
                                  best))
                              lines nil)))
      (when target
        (let* ((before (seq-take-while (lambda (l) (not (eq l target))) lines))
               (same-hunk (seq-filter (lambda (l) (= (plist-get l :hunk) (plist-get target :hunk)))
                                      before)))
          (append (last same-hunk (1- dr--excerpt-lines)) (list target)))))))

(defun dr--insert-excerpt (file excerpt)
  "Insert EXCERPT, from `dr--excerpt', as a hunk of FILE. Return its section."
  (let* ((first (car excerpt))
         (old-count (seq-count (lambda (l) (not (string-prefix-p "+" (plist-get l :text)))) excerpt))
         (new-count (seq-count (lambda (l) (not (string-prefix-p "-" (plist-get l :text)))) excerpt))
         (from (list (plist-get first :old) old-count))
         (to (list (plist-get first :new) new-count)))
    (magit-insert-section section
        (hunk (cons nil (list from to)) nil :from-range from :to-range to :combined nil)
      (magit-insert-heading
        (propertize (format "@@ -%d,%d +%d,%d @@\n" (car from) old-count (car to) new-count)
                    'font-lock-face 'magit-diff-hunk-heading))
      (dolist (line excerpt)
        (let ((text (plist-get line :text)))
          (insert (propertize (concat text "\n")
                              'font-lock-face (pcase (aref text 0)
                                                (?+ 'magit-diff-added)
                                                (?- 'magit-diff-removed)
                                                (_ 'magit-diff-context))))))
      (ignore file)
      section)))

(defun dr--review-match (comments commits)
  "Group COMMENTS by the commit each is on, as an alist from hash to comments, in the order of
COMMITS. Comments on no commit in COMMITS go last, under nil."
  (let ((infos (make-hash-table :test #'equal))
        (groups nil)
        (left comments))
    ;; Matching by hash first saves computing a patch-id for every commit.
    (dolist (sha commits)
      (let ((mine (seq-filter (lambda (c) (equal (alist-get 'commit c) sha)) left)))
        (when mine
          (puthash sha (dr--commit-info sha) infos)
          (push (cons sha mine) groups)
          (setq left (seq-difference left mine #'eq)))))
    (when left
      (dolist (sha commits)
        (let* ((info (or (gethash sha infos) (dr--commit-info sha)))
               (mine (seq-filter (lambda (c) (dr--matches-commit-p c info)) left)))
          (when mine
            (puthash sha info infos)
            (setf (alist-get sha groups nil nil #'equal)
                  (append (alist-get sha groups nil nil #'equal) mine))
            (setq left (seq-difference left mine #'eq))))))
    (setq dr--review-infos infos)
    (append (seq-filter #'car (mapcar (lambda (sha) (assoc sha groups)) commits))
            (and left (list (cons nil left))))))

(defun dr/review-refresh-buffer ()
  "Insert the branch's commits which have comments, and their files and excerpts. Record where
each comment goes in `dr--review-places', for `dr--render' to insert them."
  (let* ((comments (dr--read-store))
         (groups (dr--review-match comments (dr--review-commits)))
         (places nil))
    (magit-set-header-line-format
     (format "Review: %d %s" (length comments) (if (= (length comments) 1) "comment" "comments")))
    (magit-insert-section (review)
      (if (null comments)
          (insert "No review comments.\n")
        (pcase-dolist (`(,sha . ,mine) groups)
          (magit-insert-section commit-section (commit sha)
            (magit-insert-heading
              (if sha
                  (concat (propertize (magit-rev-abbrev sha) 'font-lock-face 'magit-hash) " "
                          (propertize (plist-get (gethash sha dr--review-infos) :subject)
                                      'font-lock-face 'magit-section-heading))
                (propertize "Comments on commits which aren't on this branch"
                            'font-lock-face 'magit-section-heading)))
            (dolist (c (seq-filter (lambda (c) (equal (alist-get 'kind c) "commit")) mine))
              (push (list c (point) commit-section (null sha)) places))
            (let* ((files (and sha (magit-git-lines "diff-tree" "--no-commit-id" "--name-only"
                                                    "-r" "-M" sha)))
                   (by-file (seq-filter (lambda (c) (alist-get 'file c)) mine))
                   (ordered (append (seq-filter (lambda (f) (seq-find (lambda (c) (equal (alist-get 'file c) f))
                                                                      by-file))
                                                files)
                                    (seq-difference (delete-dups (mapcar (lambda (c) (alist-get 'file c))
                                                                         by-file))
                                                    files))))
              (dolist (file ordered)
                (dr--insert-review-file sha file
                                        (seq-filter (lambda (c) (equal (alist-get 'file c) file))
                                                    by-file)
                                        (lambda (place) (push place places)))))
            (insert "\n")))))
    (setq dr--review-places (nreverse places))))

(defun dr--insert-review-file (sha file comments add-place)
  "Insert FILE's section, with an excerpt for each of its line COMMENTS on commit SHA, and call
ADD-PLACE with where each comment goes."
  (magit-insert-section file-section (file file)
    (magit-insert-heading
      (propertize (concat file "\n") 'font-lock-face 'magit-diff-file-heading))
    (let* ((lines (and sha (dr--diff-lines sha file)))
           (excerpts nil))
      (dolist (c comments)
        (let ((excerpt (and (equal (alist-get 'kind c) "line") (dr--excerpt lines c))))
          (if excerpt
              (push (cons c excerpt) excerpts)
            ;; File comments, and line comments whose line is gone, go under the heading.
            (funcall add-place (list c (point) file-section
                                     (not (equal (alist-get 'kind c) "file")))))))
      ;; In the order of the lines in the diff.
      (setq excerpts (sort (nreverse excerpts)
                           (lambda (a b) (< (seq-position lines (car (last (cdr a))))
                                            (seq-position lines (car (last (cdr b))))))))
      (pcase-dolist (`(,c . ,excerpt) excerpts)
        (let ((hunk (dr--insert-excerpt file excerpt)))
          (funcall add-place (list c (point) hunk nil)))))))

(defun dr/review-visit ()
  "Show the commit at point, at the comment at point or the comment below the excerpt at point."
  (interactive)
  (let* ((section (magit-current-section))
         (cid (or (get-text-property (point) 'dr-block)
                  (and (eq (oref section type) 'hunk)
                       (when-let* ((pos (dr--any-p (oref section start) (oref section end))))
                         (get-text-property pos 'dr-block)))))
         (commit section))
    (while (and commit (not (eq (oref commit type) 'commit)))
      (setq commit (oref commit parent)))
    (unless (and commit (oref commit value))
      (user-error "Not on a commit"))
    (magit-show-commit (oref commit value))
    (when cid
      (let ((buffer (magit-get-mode-buffer 'magit-revision-mode)))
        (with-current-buffer buffer
          (when-let* ((region (dr--block-region cid)))
            (goto-char (car region))
            (forward-line -1)
            (dolist (window (get-buffer-window-list buffer nil t))
              (set-window-point window (point)))))))))

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
  "Where point is: (:unit ID OFFSET) if it's in editable text, (:block CID LINES) if it's on the
read-only text of a thread, or else a marker which stays after comments inserted at it."
  (let ((unit (dr--comment-id-at (point)))
        (cid (and (< (point) (point-max)) (get-text-property (point) 'dr-block))))
    (cond (unit (list :unit unit (- (point) (car (dr--region unit)))))
          (cid (list :block cid (count-lines (car (dr--block-region cid))
                                             (line-beginning-position))))
          (t (copy-marker (point) t)))))

(defun dr--goto-spot (spot)
  (pcase spot
    ((pred markerp)
     (goto-char spot)
     (set-marker spot nil))
    (`(:unit ,unit ,offset)
     (when-let* ((region (dr--region unit)))
       (goto-char (min (+ (car region) offset) (1- (cdr region))))))
    (`(:block ,cid ,lines)
     (when-let* ((region (dr--block-region cid)))
       (goto-char (car region))
       (forward-line lines)
       (when (>= (point) (cdr region))
         (goto-char (cdr region))
         (forward-line -1))))))

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
      (unless (derived-mode-p 'dr/review-mode)
        (setq dr--commit (dr--commit-info)))
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
          (let ((next (or (next-single-property-change pos 'dr-face nil end) end)))
            (put-text-property pos next 'font-lock-face (get-text-property pos 'dr-face))
            (remove-text-properties pos next '(face nil))
            (setq pos next)))))))

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
