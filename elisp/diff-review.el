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
;; - Comments on commits only appear in revision buffers (`o' on a commit in the log), not in the
;;   diffs in the status buffer.
;; - Comments can also go on the lines of a file as it is now, in the file's buffer, for reviewing
;;   a doc or code which isn't yet committed. See "Comments on files" below.
;; - The keys for comments are under the leader key's "a" (annotate), in revision buffers and file
;;   buffers alike: aa comments or replies, ar resolves, ax deletes a thread, al lists them all.
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
;; - In the review buffer, a blank line comes before and after each thread, to set it apart from
;;   the excerpts and headings around it. When the diff line above a thread is itself blank, it
;;   serves as the blank line before, as margins collapse in CSS.
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
;;
;; A comment on a file's text has "kind" "text", and no commit:
;;   {"id": "5b1e0d2a", "kind": "text", "file": "docs/plan.md", "line": 12,
;;    "code": "the line's text", "head": "<hash>", "dirty": true, "comment": "...",
;;    "thread": [...], "resolved": false}
;; "head" is the commit HEAD was at when I made the comment, and "dirty" is there when the file
;; differed from it, so that the comment can be read later against the file it was about.
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

(defvar-local dr--outside-settings nil
  "While point is in a comment, the values of `auto-fill-function', `evil-auto-indent', and
`electric-indent-mode' to restore when it leaves.")

(defvar dr/file-mode)
(defvar dr--store-path)
(defvar dr--file)
(defvar dr--change-in-comment)
(defvar dr--store-mtime)

(defvar dr--quiet nil
  "Non-nil while the comments are taken out and put back, which aren't edits.")

;;
;; The review file.

(defun dr--store-file ()
  (or dr--store-path
      (expand-file-name "review.json" (magit-gitdir))))

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
        (delete-file file)))
    (when dr/file-mode
      (setq dr--store-mtime (dr--store-mtime-now)))))

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
      (append (list 'dr-comment unit
                    'dr-block cid
                    'magit-section section
                    'font-lock-face 'dr/comment
                    'dr-face 'dr/comment
                    'line-prefix bar
                    'wrap-prefix bar)
              (dr--file-props)))))

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
           (append props (dr--file-props)))))

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
  (cond
   (dr/file-mode
    (mapcar (lambda (comment) (cons comment (dr--text-place comment)))
            (seq-filter (lambda (comment)
                          (and (dr--text-comment-p comment)
                               (equal (alist-get 'file comment) dr--file)))
                        (dr--read-store))))
   ((derived-mode-p 'dr/review-mode)
    dr--review-places)
   (t
    ;; A comment whose commit is still on the branch stays there. Otherwise a fixup's comments
    ;; would also show on every other fixup of the same commit, since they share its subject.
    (let ((branch (dr--review-commits)))
      (mapcar (lambda (comment) (cons comment (dr--place comment)))
              (seq-filter (lambda (comment)
                            (let ((sha (alist-get 'commit comment)))
                              (and (not (dr--text-comment-p comment))
                                   (or (equal sha (plist-get dr--commit :sha))
                                       (and (not (member sha branch))
                                            (dr--matches-commit-p comment dr--commit))))))
                          (dr--read-store)))))))

(defun dr--render ()
  "Insert the comments on this buffer's commit, and make all other text read-only. In a file
buffer, insert the file's comments."
  (if dr/file-mode
      (dr--render-file)
    (dr--render-commit)))

(defun dr--render-commit ()
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
      (let* ((cid (alist-get 'id comment))
             (gap (and (derived-mode-p 'dr/review-mode) (not (dr--blank-line-before-p pos))
                       (propertize "\n" 'read-only t 'dr-block cid 'dr-gap t
                                   'magit-section section))))
        (dr--insert (concat gap (dr--block-text comment)) pos section)
        (when (alist-get 'resolved comment)
          (dr--mark-resolved (if gap (1+ pos) pos)))))))

(defun dr--blank-line-before-p (pos)
  "Whether the line before POS is blank, or a diff line of a blank line."
  (save-excursion
    (goto-char pos)
    (and (zerop (forward-line -1))
         (looking-at-p "[-+ ]?[ \t]*$"))))

(defun dr--skip-gap ()
  "Move off the blank line before a thread in the review buffer, onto the thread's first line."
  (when (get-text-property (point) 'dr-gap)
    (forward-line 1)))

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
  (if dr/file-mode
      (dr--remove-all-file)
    (let ((inhibit-read-only t)
          (dr--quiet t)
          (buffer-undo-list t)
          (pos (point-min)))
      (while (setq pos (text-property-not-all pos (point-max) 'dr-block nil))
        (delete-region pos (or (next-single-property-change pos 'dr-block) (point-max)))))))

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
    (when dr/file-mode
      (setq dr--change-in-comment
            (and (not (buffer-modified-p))
                 (or (= beg end)
                     (when-let* ((unit (get-text-property beg 'dr-comment)))
                       (dr--equal-run-p beg end 'dr-comment unit))))))
    (setq dr--chars-tick (buffer-chars-modified-tick))
    ;; A deletion can remove all of a comment's text, after which the comment can't be found.
    (setq dr--changing (dr--ids-near beg end))))

(defun dr--after-change (beg end len)
  (if dr/file-mode
      (dr--file-after-change beg end len)
    (dr--commit-after-change beg end len)))

(defun dr--commit-after-change (beg end _len)
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
    (if dr/file-mode
        (dr--file-delete beg (cdr region))
      (delete-region beg (cdr region)))
    ;; Back to the line before, as when cancelling an `o' in vim.
    (when was-in
      (goto-char beg)
      (forward-line -1)))
  ;; A file buffer's undo history was adjusted for the deletion instead.
  (unless dr/file-mode
    (setq buffer-undo-list nil)))

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
  "Whether COMMENT, which is right below where a comment at ANCHOR would go, is about the same
thing. In a file buffer, a comment below a line is always about it."
  (let-alist comment
    (and (equal .kind (plist-get anchor :kind))
         (or (equal .kind "text")
             (and (equal .file (plist-get anchor :file))
                  (equal .side (plist-get anchor :side))
                  (equal .line (plist-get anchor :line)))))))

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
          (dr--quiet-insert (concat (dr--fixed cid "── You\n" '(dr/reply-header dr/comment)
                                               'dr-header reply-id)
                                    (apply #'propertize "\n" (dr--props reply-id)))
                            end (car (gethash cid dr--placements)))
          (push reply-id dr--dirty)
          (goto-char (car (dr--region reply-id))))))))

(defun dr/open-comment ()
  "Open a comment below the line at point, and start typing it. On a line of a hunk, the comment is
about that line. On a file's heading, it's about the whole file, and in the commit's headers or
message, the whole commit. If there's already a comment there, open a new line at its top, or if
it has a thread, reply to it. On a thread, reply to it."
  (interactive)
  (unless (or dr/file-mode (derived-mode-p 'magit-revision-mode))
    (user-error "Comments go in a revision buffer, or a file's buffer"))
  (if-let* ((cid (get-text-property (point) 'dr-block)))
      (dr--reply cid)
    (let* ((anchor (if dr/file-mode (dr--file-anchor-at-point) (dr--anchor-at-point)))
           (pos (plist-get anchor :pos))
           (existing (and (< pos (point-max)) (get-text-property pos 'dr-block)))
           (existing-comment (and existing (gethash existing dr--comments)))
           (inhibit-read-only t))
      (cond
       ((not (and existing-comment (dr--same-anchor-p existing-comment anchor)))
        (let* ((id (dr--new-id))
               (comment (if dr/file-mode
                            (append `((id . ,id)
                                      (kind . "text")
                                      (file . ,dr--file)
                                      (line . ,(plist-get anchor :line))
                                      (code . ,(plist-get anchor :code)))
                                    (dr--file-version)
                                    '((comment . "")))
                          (let ((info (dr--commit-at-point)))
                            `((id . ,id)
                              (kind . ,(plist-get anchor :kind))
                              (commit . ,(plist-get info :sha))
                              (subject . ,(plist-get info :subject))
                              (patch_id . ,(plist-get info :patch-id))
                              (file . ,(plist-get anchor :file))
                              (side . ,(plist-get anchor :side))
                              (line . ,(plist-get anchor :line))
                              (code . ,(plist-get anchor :code))
                              (comment . ""))))))
          (puthash id comment dr--comments)
          (puthash id (cons id nil) dr--units)
          (puthash id (cons (plist-get anchor :section) dr--bar) dr--placements)
          (let ((gap (and (plist-get anchor :eof) (dr--eof-gap id))))
            (dr--quiet-insert (concat gap (apply #'propertize "\n" (dr--props id)))
                              pos (plist-get anchor :section))
            (when dr/file-mode
              (dr--fix-blocks))
            (push id dr--dirty)
            (goto-char (+ pos (length gap))))))
       ((or (alist-get 'thread existing-comment) (alist-get 'resolved existing-comment))
        (dr--reply existing))
       (t
        ;; A new line at the top of the comment. In a file buffer, text typed where a comment
        ;; begins goes to the comment only when point is in it.
        (let ((start (car (dr--region existing)))
              (dr--in-comment t))
          (goto-char start)
          (insert "\n")
          (goto-char start))))))
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
        (push (dr--past-gap pos) starts))
      (setq pos (next-single-property-change pos 'dr-block nil (point-max))))
    (nreverse starts)))

(defun dr/any-comments-p ()
  (and (or dr/file-mode (derived-mode-p 'magit-revision-mode)) (dr--any-p) t))

(defun dr--past-gap (pos)
  "POS, the start of a comment's block, or the line after it if it's the blank line before a
thread in the review buffer. This is where the comment's navigation stops."
  (if (get-text-property pos 'dr-gap) (1+ pos) pos))

(defun dr--here ()
  "Point, or if it's in a comment's block, where the block's navigation stops."
  (if-let* ((cid (get-text-property (point) 'dr-block)))
      (dr--past-gap (car (dr--block-region cid)))
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
  (dr--show-commit-elsewhere (get-text-property (point) 'dr-rev)
                             (not (derived-mode-p 'dr/review-mode))))

(defun dr--show-commit-elsewhere (rev dedicated)
  "Show commit REV in another window, keeping the focus here, and return its buffer. With
DEDICATED, in a revision buffer of its own, rather than the repo's usual one. This picks the
window itself, overriding `magit-display-buffer-function' and `display-buffer-alist', since my
rules for revision buffers may use this window, e.g. when there's no window to its right."
  (let ((magit-display-buffer-noselect t)
        (display-buffer-overriding-action '((display-buffer-reuse-window
                                             display-buffer-use-some-window
                                             display-buffer-pop-up-window)
                                            (inhibit-same-window . t)))
        (magit-revision-use-dedicated-buffers dedicated))
    (magit-revision-setup-buffer rev (car (magit-show-commit--arguments)) nil)))

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
  ;; A file buffer's undo history was adjusted for the comments taken out and put back instead.
  (unless dr/file-mode
    (setq buffer-undo-list nil))
  (dr--update-in-comment)
  ;; With no comments left, gn and gp go back to their usual meanings.
  (when (and dr/file-mode (not (dr--any-p)))
    (dr/file-mode -1)))

(defun dr/comment ()
  "Comment on the line at point, or reply to the thread at point. In a revision buffer, this is
`dr/open-comment'. In a file's buffer, the comment is on the line of the file."
  (interactive)
  (cond ((derived-mode-p 'magit-revision-mode) (dr/open-comment))
        (buffer-file-name
         (unless dr/file-mode
           (dr/file-mode 1))
         (dr/open-comment))
        (t (user-error "Comments go in a revision buffer, or a file's buffer"))))

;;
;; Comments on files.
;;
;; In a file's buffer, `dr/file-mode' shows the comments on the file's lines below them, as a
;; revision buffer does, and the keys work the same inside them. Unlike in a revision buffer, the
;; text around the comments is the file's, and stays editable, so:
;; - The comments must never reach the file on disk. They're taken out of the buffer before it's
;;   saved, so that before-save hooks which edit the buffer (deleting trailing whitespace,
;;   formatting) see only the file's text, and put back after. Any other write of the whole buffer,
;;   such as an auto-save, writes a copy without them.
;; - Taking comments out and putting them back isn't an edit of the file, so it's not recorded for
;;   undo, and doesn't mark the buffer modified. But each change in the undo history is recorded
;;   by its positions in the buffer, so the history is adjusted for each comment taken out or put
;;   back (`dr--undo-insert', `dr--undo-delete'), so that undo still undoes my edits of the file.
;; - An edit belongs either to the file's text or to one comment's text. An edit which reaches
;;   across both, like deleting lines of the file along with a comment, is refused
;;   (`dr--guard'). Text typed where a comment and the file's text meet goes to the comment if
;;   point was in the comment, and otherwise to the file.
;; - A comment stays right below its line: the newline which ends the line is marked as the
;;   comment's anchor. Text inserted between them, like a line opened with o or pasted with p, is
;;   moved to after the comment (`dr--fix-blocks').
;; - Comment text is given neutral syntax, so that the file's mode doesn't parse a quote or a
;;   bracket in a comment as code, and it isn't fontified as code.
;; - A comment is anchored to its line's text and number, as a line comment in a commit is, so it
;;   follows its line when the file is changed outside of Emacs, e.g. by Claude. Saving updates
;;   the anchors, since my edits may have moved the lines.
;; - Claude's replies show up without a refresh: a timer checks whether the review file changed.

(defvar-local dr--file nil
  "In a buffer with `dr/file-mode', the file's path relative to the repo's top level.")

(defvar-local dr--store-path nil
  "In a buffer with `dr/file-mode', the review file's path.")

(defvar-local dr--store-mtime nil
  "In a buffer with `dr/file-mode', the review file's modification time when the buffer last read
or wrote it.")

(defvar-local dr--stripped nil
  "While the comments are out of a file buffer for saving it, where point was, from
`dr--point-spot'.")

(defvar-local dr--change-in-comment nil
  "Whether the change in progress in a file buffer is an edit of one comment's text only, in a
buffer which was unmodified, so that it stays unmodified.")

(defvar-local dr--check-blocks nil
  "Whether an edit in a file buffer may have moved a comment away from its line.")

(defvar dr--watch-timer nil
  "The timer which redraws file buffers' comments when their review file changes.")

(defconst dr--comment-props
  '(dr-comment dr-block dr-face dr-header dr-rev dr-gap dr-anchor magit-section font-lock-face
               line-prefix wrap-prefix keymap read-only rear-nonsticky modification-hooks
               syntax-table)
  "The text properties of comments in a file buffer, which text pasted into the file must not
carry.")

(defconst dr--word-syntax (string-to-syntax "w"))
(defconst dr--space-syntax (string-to-syntax " "))
(defconst dr--punctuation-syntax (string-to-syntax "."))

(define-minor-mode dr/file-mode
  "Show the review comments on this file's lines below them, and add more with `dr/comment'.
See diff-review.el."
  :lighter nil
  (if dr/file-mode
      (dr--file-mode-on)
    (dr--file-mode-off)))

(defun dr--text-comment-p (comment)
  "Whether COMMENT is on the text of a file, rather than on a commit."
  (equal (alist-get 'kind comment) "text"))

(defun dr--file-props ()
  "The text properties which comment text has in a file buffer, besides those it has in a
revision buffer. Text inserted next to a comment doesn't inherit its properties, since it may
belong to the file, and `dr--guard' refuses edits which reach outside of it."
  (when dr/file-mode
    (list 'rear-nonsticky t
          'modification-hooks (list #'dr--guard))))

(defun dr--anchor-props (cid)
  "The text properties of the newline which ends the line comment CID is on."
  (list 'dr-anchor cid
        'rear-nonsticky t
        'modification-hooks (list #'dr--guard-anchor)))

(defmacro dr--silently (&rest body)
  "Run BODY, which inserts or deletes comments in a file buffer, without it counting as an edit of
the file: it's not recorded for undo, doesn't mark the buffer modified or run change hooks, and
doesn't lock the file or check whether it changed on disk."
  (declare (indent 0) (debug t))
  `(let ((buffer-file-name nil)
         (buffer-file-truename nil)
         (dr--quiet t))
     (with-silent-modifications ,@body)))

(defun dr--equal-run-p (beg end prop value)
  "Whether every character in BEG..END has PROP equal to VALUE."
  (let ((pos beg)
        (ok t))
    (while (and ok (< pos end))
      (setq ok (equal (get-text-property pos prop) value))
      (setq pos (next-single-property-change pos prop nil end)))
    ok))

(defun dr--guard (beg end)
  "Refuse a change to BEG..END which reaches outside of one comment's or reply's text, e.g.
deleting lines of the file along with a comment. It's in the `modification-hooks' of comment
text in a file buffer, and runs before the change."
  (unless (or dr--quiet undo-in-progress)
    (let ((unit (get-text-property beg 'dr-comment)))
      (unless (and unit (dr--equal-run-p beg end 'dr-comment unit))
        (signal 'text-read-only (list "Can't edit across a review comment"))))))

(defun dr--guard-anchor (beg _end)
  "Refuse to delete the newline which ends a line with a comment below it, unless the whole line
goes with it, as with dd. Joining the line with the comment's first line would make no sense."
  (unless (or dr--quiet undo-in-progress (= beg (point-min)) (eq (char-before beg) ?\n))
    (signal 'text-read-only (list "Can't join a line with the review comment below it"))))

;; The undo history.

(defun dr--undo-insert (pos n)
  "Adjust `buffer-undo-list' for N characters about to be inserted at POS without being recorded,
e.g. a comment, so that undoing the changes before it still applies each at the right place.
Each change's positions are in the buffer as it was right after the change, so POS is carried
back through the changes in turn. The new text goes before the restored text of a deletion at
POS, and before the text of an insertion at POS."
  (when (listp buffer-undo-list)
    (let ((cell buffer-undo-list)
          (prev nil)
          (p pos))
      (while cell
        (let ((entry (car cell))
              (stop nil))
          (pcase entry
            ((pred integerp)
             (when (> entry p)
               (setcar cell (+ entry n))))
            (`(,(and b (pred integerp)) . ,(and e (pred integerp)))
             (cond ((>= b p) (setcar cell (cons (+ b n) (+ e n))))
                   ((> e p)
                    ;; The inserted text is split in two by the new text. Undo deletes the second
                    ;; part first, which leaves the first part's positions as they are.
                    (setcar cell (cons (+ p n) (+ e n)))
                    (setcdr cell (cons (cons b p) (cdr cell)))
                    (setq prev cell
                          cell (cdr cell))))
             (setq p (cond ((>= p e) (- p (- e b)))
                           ((> p b) b)
                           (t p))))
            (`(,(and text (pred stringp)) . ,(and at (pred integerp)))
             (let ((x (abs at)))
               (when (> x p)
                 (setcar cell (cons text (if (< at 0) (- (+ x n)) (+ x n)))))
               (when (>= p x)
                 (setq p (+ p (length text))))))
            (`(nil ,prop ,value ,(and b (pred integerp)) . ,(and e (pred integerp)))
             (setcar cell `(nil ,prop ,value ,(if (>= b p) (+ b n) b) . ,(if (> e p) (+ e n) e))))
            (`(apply . ,_) (setq stop t)))
          (if stop
              ;; A change which can't be adjusted. Forget it, and every change before it.
              (progn
                (if prev (setcdr prev nil) (setq buffer-undo-list nil))
                (setq cell nil))
            (setq prev cell
                  cell (cdr cell))))))))

(defun dr--undo-delete (beg end)
  "Adjust `buffer-undo-list' for BEG..END, a comment's text, about to be deleted without being
recorded. The changes inside of it are dropped from the history: they were edits of the comment,
which are saved apart from the file. See `dr--undo-insert'."
  (when (listp buffer-undo-list)
    (let ((cell buffer-undo-list)
          (prev nil)
          (a beg)
          (b end))
      (while cell
        (let ((entry (car cell))
              (n (- b a))
              (drop nil)
              (stop nil))
          (pcase entry
            ((pred integerp)
             (setcar cell (cond ((>= entry b) (- entry n))
                                ((> entry a) a)
                                (t entry))))
            (`(,(and ib (pred integerp)) . ,(and ie (pred integerp)))
             (cond ((<= ie a) (setq a (- a (- ie ib))
                                    b (- b (- ie ib))))
                   ((>= ib b) (setcar cell (cons (- ib n) (- ie n))))
                   ((and (>= ib a) (<= ie b)) (setq drop t
                                                    b (- b (- ie ib))))
                   (t (setq stop t))))
            (`(,(and text (pred stringp)) . ,(and at (pred integerp)))
             (let* ((x (abs at))
                    (len (length text))
                    (inside (or (< a x b)
                                (and (memql x (list a b))
                                     (get-text-property 0 'dr-block text)))))
               (cond (inside (setq drop t
                                   b (+ b len)))
                     ((<= x a) (setq a (+ a len)
                                     b (+ b len)))
                     (t (setcar cell (cons text (if (< at 0) (- (- x n)) (- x n))))))))
            (`(nil ,prop ,value ,(and ib (pred integerp)) . ,(and ie (pred integerp)))
             (cond ((<= ie a))
                   ((>= ib b) (setcar cell `(nil ,prop ,value ,(- ib n) . ,(- ie n))))
                   ((and (>= ib a) (<= ie b)) (setq drop t))
                   (t (setq stop t))))
            (`(apply . ,_) (setq stop t)))
          (cond (stop
                 (if prev (setcdr prev nil) (setq buffer-undo-list nil))
                 (setq cell nil))
                (drop
                 (if prev (setcdr prev (cdr cell)) (setq buffer-undo-list (cdr cell)))
                 (setq cell (cdr cell)))
                (t
                 (setq prev cell
                       cell (cdr cell))))))
      (dr--undo-tidy))))

(defun dr--undo-tidy ()
  "Remove the groups of changes in `buffer-undo-list' which no longer change anything, since
`dr--undo-delete' dropped their changes: those left empty, or with only where point was. Undo
would otherwise spend a step on each."
  (let ((cell buffer-undo-list)
        (boundary nil)
        (useful nil))
    ;; BOUNDARY is the boundary before the group CELL is in, or nil for the first group.
    (while cell
      (if (car cell)
          (unless (integerp (car cell))
            (setq useful t))
        (cond (useful (setq boundary cell))
              (boundary (setcdr boundary (cdr cell)))
              (t (setq buffer-undo-list (cdr cell))))
        (setq useful nil))
      (setq cell (cdr cell)))
    (when (and boundary (not useful))
      (setcdr boundary nil))))

;; Inserting and removing comments.

(defun dr--quiet-insert (text pos section)
  "Insert TEXT, a comment's block or part of one, at POS in SECTION, as `dr--insert' does. In a
file buffer, this isn't an edit of the file, and the undo history is adjusted for it."
  (if (not dr/file-mode)
      (let ((dr--quiet t))
        (dr--insert text pos section))
    (dr--undo-insert pos (length text))
    (dr--silently
      (dr--insert text pos section)
      (dr--neutralize-syntax pos (+ pos (length text))))
    (syntax-ppss-flush-cache pos)))

(defun dr--file-delete (beg end)
  "Delete BEG..END, comment text in a file buffer, which isn't an edit of the file."
  (dr--undo-delete beg end)
  (dr--silently
    (delete-region beg end))
  (syntax-ppss-flush-cache beg))

(defun dr--eof-gap (cid)
  "The newline which starts comment CID's block when its line is the last of the file, and has no
newline of its own."
  (propertize "\n" 'read-only t 'dr-block cid 'dr-gap t 'rear-nonsticky t
              'modification-hooks (list #'dr--guard)))

(defun dr--neutralize-syntax (beg end)
  "Give the comment text in BEG..END syntax which the file's mode can't take for code: letters,
digits, and underscores are word characters, whitespace is whitespace, and the rest is
punctuation. Otherwise, a quote in a comment would start a string which runs on into the code
after it."
  (let ((pos beg))
    (while (setq pos (dr--any-p pos end))
      (let ((stop (or (next-single-property-change pos 'dr-block nil end) end)))
        (save-excursion
          (goto-char pos)
          (while (< (point) stop)
            (let ((c (char-after)))
              (put-text-property (point) (1+ (point)) 'syntax-table
                                 (cond ((memq c '(?\s ?\t ?\n)) dr--space-syntax)
                                       ((or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9) (eq c ?_)
                                            (> c 127))
                                        dr--word-syntax)
                                       (t dr--punctuation-syntax))))
            (forward-char)))
        (setq pos stop)))))

(defun dr--around-fontify (fontify beg end &rest args)
  "Fontify BEG..END as the file's mode does, except for comment text, whose faces are its own."
  (prog1 (apply fontify beg end args)
    (with-silent-modifications
      (let ((pos beg)
            (end (min end (point-max))))
        (while (setq pos (dr--any-p pos end))
          (let ((next (or (next-single-property-change pos 'dr-block nil end) end)))
            (remove-text-properties pos next '(face nil))
            (setq pos next)))))))

(defun dr--block-start-p (pos)
  "Whether POS is the first character of a comment's block."
  (when-let* ((cid (get-text-property pos 'dr-block)))
    (or (= pos (point-min))
        (not (equal (get-text-property (1- pos) 'dr-block) cid)))))

(defun dr--blocks ()
  "The comment blocks in the buffer, as a list of (CID BEG END), in buffer order."
  (let ((pos (point-min))
        blocks)
    (while (setq pos (dr--any-p pos))
      (let ((end (or (next-single-property-change pos 'dr-block) (point-max))))
        (push (list (get-text-property pos 'dr-block) pos end) blocks)
        (setq pos end)))
    (nreverse blocks)))

(defun dr--anchors (cid)
  "The positions of the newlines marked as comment CID's anchor."
  (save-excursion
    (goto-char (point-min))
    (let (anchors match)
      (while (setq match (text-property-search-forward 'dr-anchor cid t))
        (push (prop-match-beginning match) anchors))
      (nreverse anchors))))

(defun dr--set-anchor (cid pos)
  "Make the newline at POS comment CID's only anchor."
  (dr--silently
    (dolist (anchor (dr--anchors cid))
      (unless (eql anchor pos)
        (remove-list-of-text-properties anchor (1+ anchor)
                                        '(dr-anchor rear-nonsticky modification-hooks))))
    (when pos
      (add-text-properties pos (1+ pos) (dr--anchor-props cid)))))

(defun dr--move-block (beg end to)
  "Move the comment block BEG..END to TO, outside of it. If point is in the block, it moves with
it. If it's at TO, it ends up after the block, as it would after text inserted at point."
  (let* ((text (buffer-substring beg end))
         (cid (get-text-property beg 'dr-block))
         (offset (and (<= beg (point)) (< (point) end) (- (point) beg)))
         (after (unless offset (copy-marker (point) t)))
         (target (if (> to beg) (- to (- end beg)) to)))
    (dr--file-delete beg end)
    (dr--quiet-insert text target nil)
    (when (alist-get 'resolved (gethash cid dr--comments))
      (dr--mark-resolved (+ target (if (get-text-property target 'dr-gap) 1 0))))
    (if offset
        (goto-char (+ target offset))
      (goto-char after)
      (set-marker after nil))))

(defun dr--fix-blocks ()
  "Keep each comment's block right below its line, after an edit near it. The newline ending the
line is marked as the block's anchor. If text was inserted between them, e.g. a line opened with
o or pasted with p, the block moves up, to below its line again. If the line was deleted, the
block stays below the line above, which becomes its anchor."
  ;; From the last block to the first, so that moving a block doesn't move the ones still to do.
  (pcase-dolist (`(,cid ,beg ,end) (reverse (dr--blocks)))
    (let ((anchors (dr--anchors cid)))
      (cond
       ;; At the top of the file, or after its last line, there's no newline to anchor to.
       ((or (= beg (point-min)) (get-text-property beg 'dr-gap))
        (dr--set-anchor cid nil))
       ((equal (get-text-property (1- beg) 'dr-anchor) cid)
        (dr--set-anchor cid (1- beg)))
       ;; The block is in the middle of a line, e.g. after undoing the line's newline: put it after
       ;; that line.
       ((not (eq (char-before beg) ?\n))
        (when-let* ((newline (save-excursion
                               (goto-char end)
                               (and (search-forward "\n" nil t) (1- (point))))))
          (dr--move-block beg end (1+ newline))
          (dr--set-anchor cid (- newline (- end beg)))))
       ((when-let* ((anchor (car (last (seq-filter (lambda (a) (< a beg)) anchors)))))
          (unless (dr--any-p (1+ anchor) beg)
            (dr--move-block beg end (1+ anchor))
            (dr--set-anchor cid anchor)
            t)))
       (t (dr--set-anchor cid (1- beg)))))))

(defun dr--text-place (comment)
  "Where text COMMENT goes in this file buffer, which has no comments in it yet, as (POS SECTION
ORPHAN EOF): below the line with its line's text nearest its line number, or at the top of the
file, as an orphan, if there's no such line. EOF is non-nil when that line is the file's last,
and has no newline."
  (let-alist comment
    (let ((case-fold-search nil)
          (regexp (concat "^" (regexp-quote (or .code "")) "$"))
          (line 1)
          (from (point-min))
          (done nil)
          best
          best-distance)
      (save-excursion
        (goto-char (point-min))
        (while (and (not done)
                    (re-search-forward regexp nil t)
                    ;; The empty "line" after the file's final newline isn't a line.
                    (not (and (= (match-beginning 0) (point-max)) (not (bobp)))))
          (let ((bol (match-beginning 0)))
            (setq line (+ line (count-lines from bol))
                  from bol)
            (let ((distance (abs (- line (or .line 1)))))
              (when (or (null best) (< distance best-distance))
                (setq best (match-end 0)
                      best-distance distance))))
          (if (eobp)
              (setq done t)
            (forward-line 1)))
        (cond ((null best) (list (point-min) nil t nil))
              ((= best (point-max)) (list best nil nil t))
              (t (list (1+ best) nil nil nil)))))))

(defun dr--render-file ()
  "Insert the comments on this buffer's file, below their lines."
  (setq dr--store-mtime (dr--store-mtime-now))
  (setq dr--comments (make-hash-table :test #'equal))
  (setq dr--placements (make-hash-table :test #'equal))
  (setq dr--units (make-hash-table :test #'equal))
  (let ((placed nil)
        (i 0))
    (pcase-dolist (`(,comment . ,place) (dr--places))
      (puthash (alist-get 'id comment) comment dr--comments)
      (push (cons (cl-incf i (if (nth 2 place) 100000 1)) (cons comment place)) placed))
    ;; As in `dr--render-commit', from the end of the buffer backwards.
    (setq placed (sort placed (lambda (a b)
                                (let ((pos-a (nth 2 a))
                                      (pos-b (nth 2 b)))
                                  (or (> pos-a pos-b)
                                      (and (= pos-a pos-b) (> (car a) (car b))))))))
    (pcase-dolist (`(,_ ,comment ,pos ,_ ,orphan ,eof) placed)
      (let* ((cid (alist-get 'id comment))
             (gap (and eof (dr--eof-gap cid))))
        (puthash cid (cons nil (if orphan dr--orphan-bar dr--bar)) dr--placements)
        (dr--quiet-insert (concat gap (dr--block-text comment)) pos nil)
        (when (alist-get 'resolved comment)
          (dr--mark-resolved (+ pos (length gap))))))
    (dr--fix-blocks)))

(defun dr--remove-all-file ()
  "Delete every comment from this file buffer, and their anchors."
  (pcase-dolist (`(,cid ,beg ,end) (reverse (dr--blocks)))
    (dr--file-delete beg end)
    (dr--set-anchor cid nil)))

(defun dr--text-without-comments ()
  "The buffer's text without the comments, which is the file's."
  (let ((pos (point-min))
        (parts nil))
    (pcase-dolist (`(,_ ,beg ,end) (dr--blocks))
      (push (buffer-substring-no-properties pos beg) parts)
      (setq pos end))
    (push (buffer-substring-no-properties pos (point-max)) parts)
    (apply #'concat (nreverse parts))))

(defun dr--file-line (pos)
  "The number of the line at POS, a position in the file's text, in the file without comments."
  (let ((line (line-number-at-pos pos)))
    (pcase-dolist (`(,_ ,beg ,end) (dr--blocks))
      (when (< beg pos)
        (setq line (- line (count-lines beg (min end pos))))))
    line))

(defun dr--file-version ()
  "Which version of the file a comment opened now is on, as alist fields: `head', the commit HEAD
is at, if any, and `dirty', present when the file differs from it, with unsaved or uncommitted
edits, or isn't committed at all. So that an archived comment can be read later against the file
it was about."
  (let ((head (magit-rev-parse "HEAD")))
    (append (and head `((head . ,head)))
            (and (or (buffer-modified-p)
                     (null head)
                     (magit-git-string "status" "--porcelain" "--" dr--file))
                 '((dirty . t))))))

(defun dr--file-anchor-at-point ()
  "What a comment opened at point in a file buffer is about, as a plist like that of
`dr--anchor-at-point': the line at point."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position)))
    (list :kind "text" :file dr--file :line (dr--file-line bol)
          :code (buffer-substring-no-properties bol eol)
          :pos (if (or (= eol (point-max)) (get-text-property eol 'dr-block)) eol (1+ eol))
          :eof (= eol (point-max))
          :section nil)))

(defun dr--file-after-change (beg end _len)
  "After an edit of a file buffer: decide whether inserted text belongs to the file or to a
comment, and note which comments changed."
  (unless (or dr--quiet (eql dr--chars-tick (buffer-chars-modified-tick)))
    (let ((ids dr--changing))
      (when (< beg end)
        (let* ((before (and (> beg (point-min)) (dr--comment-id-at (1- beg))))
               (after (and (< end (point-max)) (dr--comment-id-at end)))
               (in-block (and (< end (point-max)) (get-text-property end 'dr-block)))
               (owner (cond
                       ;; Text which undo restores has the properties it had.
                       (undo-in-progress (get-text-property beg 'dr-comment))
                       ((and before (equal before after)) before)
                       ;; Where the line above a comment ends and the comment begins.
                       ((and after (dr--block-start-p end)) (and dr--in-comment after))
                       (after after)
                       ;; At the end of a comment, before a reply's header.
                       ((and before in-block) before)
                       ;; Where a comment ends and the line below it begins.
                       (before (and dr--in-comment before)))))
          (unless undo-in-progress
            (dr--silently
              (if owner
                  (progn
                    (set-text-properties beg end (dr--props owner))
                    (dr--neutralize-syntax beg end))
                (remove-list-of-text-properties beg end dr--comment-props))))
          (if owner
              (push owner ids)
            (setq dr--change-in-comment nil))
          ;; A newline typed at the end of a line with a comment, as with o, ends that line, and
          ;; the comment stays below it.
          (when-let* (((not owner))
                      (cid (and (< end (point-max)) (get-text-property end 'dr-anchor)))
                      (newline (save-excursion
                                 (goto-char beg)
                                 (and (search-forward "\n" end t) (1- (point))))))
            (dr--set-anchor cid newline))))
      (setq ids (delete-dups ids))
      (setq dr--dirty (delete-dups (append ids dr--dirty)))
      (dolist (id ids)
        (when-let* ((region (dr--region id)))
          ;; A comment must end with a newline, as in a revision buffer.
          (unless (eq (char-before (cdr region)) ?\n)
            (save-excursion
              (goto-char (cdr region))
              (insert (apply #'propertize "\n" (dr--props id))))
            (dr--silently
              (dr--neutralize-syntax (cdr region) (1+ (cdr region)))))))
      ;; Editing a comment isn't editing the file.
      (when dr--change-in-comment
        (set-buffer-modified-p nil))
      (setq dr--check-blocks t))))

(defun dr--auto-fill ()
  "Auto-fill in a comment in a file buffer, without the file's mode's comment prefixes or
indentation."
  (let ((comment-line-break-function (lambda (&optional _) (insert "\n")))
        (adaptive-fill-mode nil)
        (fill-prefix nil)
        (comment-auto-fill-only-comments nil))
    (do-auto-fill)))

;; Saving and reverting.

(defun dr--update-anchors ()
  "Record in the review file the line each comment in this file buffer is below now, since edits
may have moved it."
  (let ((changes nil))
    (pcase-dolist (`(,cid ,beg ,_) (dr--blocks))
      (unless (or (= beg (point-min))
                  (eq (cdr (gethash cid dr--placements)) dr--orphan-bar))
        (save-excursion
          (goto-char (1- beg))
          (let* ((bol (line-beginning-position))
                 (line (dr--file-line bol))
                 (code (buffer-substring-no-properties bol (line-end-position)))
                 (comment (gethash cid dr--comments)))
            (unless (and (equal line (alist-get 'line comment))
                         (equal code (alist-get 'code comment)))
              (push (list cid line code) changes))))))
    (when changes
      (let ((store (dr--read-store)))
        (pcase-dolist (`(,cid ,line ,code) changes)
          (when-let* ((index (seq-position store cid (lambda (c id) (equal (alist-get 'id c) id)))))
            (let ((comment (copy-alist (nth index store))))
              (setf (alist-get 'line comment) line)
              (setf (alist-get 'code comment) code)
              (setf (nth index store) comment)
              (puthash cid comment dr--comments))))
        (dr--write-store store)))))

(defun dr--before-save ()
  "Take the comments out of the buffer before it's saved, so that the hooks which edit the buffer
before saving only see the file's text."
  (when (and dr/file-mode (not dr--stripped))
    (dr--save)
    (dr--update-anchors)
    (setq dr--stripped (dr--point-spot))
    (dr--remove-all-file)))

(defun dr--after-save ()
  "Put the comments back after saving."
  (when (and dr/file-mode dr--stripped)
    (let ((spot dr--stripped))
      (setq dr--stripped nil)
      (dr--render-file)
      (dr--goto-spot spot))
    (dr--update-in-comment)))

(defun dr--annotate-write (start end)
  "When the whole buffer is written with its comments in it, as by an auto-save, write a copy
without them. This is in `write-region-annotate-functions'."
  (when (and dr/file-mode
             (or (null start) (and (eql start (point-min)) (eql end (point-max))))
             (dr--any-p))
    (let ((text (dr--text-without-comments))
          (coding buffer-file-coding-system))
      (set-buffer (generate-new-buffer " *dr-write*" t))
      (setq buffer-file-coding-system coding)
      (insert text)
      (setq-local write-region-post-annotation-function #'kill-buffer)))
  nil)

(defun dr--before-revert ()
  (when dr/file-mode
    (dr--save)
    (dr--remove-all-file)))

(defun dr--after-revert ()
  (if dr/file-mode
      (dr--render-file)
    ;; Reverting may have reset the buffer's modes.
    (dr--maybe-enable)))

;; Turning comments on.

(defun dr--file-mode-on ()
  (let ((root (and buffer-file-name (magit-toplevel))))
    (unless root
      (setq dr/file-mode nil)
      (user-error "Comments go on files in a git repo"))
    (setq dr--file (file-relative-name (file-truename buffer-file-name) (file-truename root)))
    (setq dr--store-path (expand-file-name "review.json" (magit-gitdir))))
  (setq-local yank-excluded-properties (if (eq yank-excluded-properties t)
                                           t
                                         (append dr--comment-props yank-excluded-properties)))
  (setq-local parse-sexp-lookup-properties t)
  (when syntax-propertize-function
    (add-function :after (local 'syntax-propertize-function) #'dr--neutralize-syntax))
  (add-function :around (local 'font-lock-fontify-region-function) #'dr--around-fontify)
  (add-hook 'before-change-functions #'dr--before-change nil t)
  (add-hook 'after-change-functions #'dr--after-change nil t)
  (add-hook 'post-command-hook #'dr--post-command nil t)
  (add-hook 'kill-buffer-hook #'dr--save nil t)
  (add-hook 'before-save-hook #'dr--before-save -100 t)
  (add-hook 'after-save-hook #'dr--after-save 100 t)
  (add-hook 'write-region-annotate-functions #'dr--annotate-write nil t)
  (dr--render-file)
  (unless dr--watch-timer
    (setq dr--watch-timer (run-with-timer 2 2 #'dr--check-stores))))

(defun dr--file-mode-off ()
  (dr--save)
  (dr--remove-all-file)
  (when dr--in-comment
    (setq dr--in-comment nil)
    (use-local-map dr--magit-map)
    (pcase-let ((`(,fill ,auto-indent ,electric) dr--outside-settings))
      (setq auto-fill-function fill)
      (setq-local evil-auto-indent auto-indent)
      (setq-local electric-indent-mode electric))
    (evil-normalize-keymaps))
  (kill-local-variable 'yank-excluded-properties)
  (kill-local-variable 'parse-sexp-lookup-properties)
  (remove-function (local 'syntax-propertize-function) #'dr--neutralize-syntax)
  (remove-function (local 'font-lock-fontify-region-function) #'dr--around-fontify)
  (remove-hook 'before-change-functions #'dr--before-change t)
  (remove-hook 'after-change-functions #'dr--after-change t)
  (remove-hook 'post-command-hook #'dr--post-command t)
  (remove-hook 'kill-buffer-hook #'dr--save t)
  (remove-hook 'before-save-hook #'dr--before-save t)
  (remove-hook 'after-save-hook #'dr--after-save t)
  (remove-hook 'write-region-annotate-functions #'dr--annotate-write t))

(defun dr--maybe-enable ()
  "In a file's buffer, turn on `dr/file-mode' if the file has review comments. This is in
`find-file-hook', so it avoids running git unless the repo has a review file."
  (when-let* (((and buffer-file-name (not dr/file-mode)))
              (root (locate-dominating-file buffer-file-name ".git"))
              (dot-git (expand-file-name ".git" root))
              (store (if (file-directory-p dot-git)
                         (expand-file-name "review.json" dot-git)
                       (let ((default-directory root))
                         (expand-file-name "review.json" (magit-gitdir)))))
              ((file-exists-p store))
              (file (file-relative-name (file-truename buffer-file-name) (file-truename root))))
    (when (seq-find (lambda (c) (and (dr--text-comment-p c) (equal (alist-get 'file c) file)))
                    (let ((default-directory root)) (dr--read-store)))
      (dr/file-mode 1))))

(defun dr--store-mtime-now ()
  (file-attribute-modification-time (file-attributes (dr--store-file))))

(defun dr--shown-comments-changed-p ()
  "Whether this file buffer's comments in the review file differ from those it shows."
  (let ((now (seq-filter (lambda (c) (and (dr--text-comment-p c)
                                          (equal (alist-get 'file c) dr--file)))
                         (dr--read-store))))
    (or (/= (length now) (hash-table-count dr--comments))
        (seq-some (lambda (c) (not (equal c (gethash (alist-get 'id c) dr--comments)))) now))))

(defun dr--check-stores ()
  "Redraw the comments of each file buffer whose review file changed since the buffer last read or
wrote it, e.g. with Claude's replies. A buffer where I'm typing a comment waits until I'm done."
  (let ((buffers (seq-filter (lambda (b) (buffer-local-value 'dr/file-mode b)) (buffer-list))))
    (if (null buffers)
        (progn
          (cancel-timer dr--watch-timer)
          (setq dr--watch-timer nil))
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (let ((mtime (dr--store-mtime-now)))
            (unless (or (equal mtime dr--store-mtime)
                        dr--stripped
                        (and dr--in-comment (memq evil-state '(insert replace))))
              (setq dr--store-mtime mtime)
              (when (dr--shown-comments-changed-p)
                (dr--rerender)))))))))

(add-hook 'find-file-hook #'dr--maybe-enable)
(add-hook 'before-revert-hook #'dr--before-revert)
(add-hook 'after-revert-hook #'dr--after-revert)

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
        (left (seq-remove #'dr--text-comment-p comments)))
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
            (when-let* ((commit-comments (seq-filter (lambda (c) (equal (alist-get 'kind c) "commit"))
                                                     mine)))
              (dolist (c commit-comments)
                (push (list c (point) commit-section (null sha)) places))
              (dr--insert-gap))
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
                                        (lambda (place) (push place places))))))))
      (when-let* ((text-comments (seq-filter #'dr--text-comment-p comments)))
        (dr--insert-review-text text-comments (lambda (place) (push place places)))))
    (setq dr--review-places (nreverse places))))

(defun dr--text-excerpt (comment)
  "In a buffer with a file's text, the excerpt to show above text COMMENT in the review buffer, as
(LINE . TEXT), where LINE is the commented line's number. Nil if its line isn't there."
  (pcase-let ((`(,pos ,_ ,orphan ,eof) (dr--text-place comment)))
    (unless orphan
      (let* ((end (if eof pos (1- pos)))
             (start (save-excursion
                      (goto-char end)
                      (forward-line (- (1- dr--excerpt-lines)))
                      (point))))
        (cons (line-number-at-pos end) (buffer-substring-no-properties start end))))))

(defun dr--insert-review-text (comments add-place)
  "Insert a section with the COMMENTS on files' text, by file, each after an excerpt of the lines
above it in the file as it is now. Call ADD-PLACE with where each comment goes."
  (let ((root (magit-toplevel)))
    (magit-insert-section (dr-files)
      (magit-insert-heading
        (propertize "Comments on files" 'font-lock-face 'magit-section-heading))
      (dolist (file (sort (delete-dups (mapcar (lambda (c) (alist-get 'file c)) comments))
                          #'string<))
        (let* ((mine (seq-filter (lambda (c) (equal (alist-get 'file c) file)) comments))
               (path (expand-file-name file root))
               (excerpts (with-temp-buffer
                           (when (file-exists-p path)
                             (insert-file-contents path))
                           (mapcar (lambda (c) (cons c (dr--text-excerpt c))) mine)))
               (orphans (seq-remove #'cdr excerpts)))
          (magit-insert-section file-section (file file)
            (magit-insert-heading
              (propertize (concat file "\n") 'font-lock-face 'magit-diff-file-heading))
            (pcase-dolist (`(,c . ,_) orphans)
              (funcall add-place (list c (point) file-section t)))
            (when orphans
              (dr--insert-gap))
            (pcase-dolist (`(,c . (,_ . ,text))
                           (sort (seq-filter #'cdr excerpts)
                                 (lambda (a b) (< (cadr a) (cadr b)))))
              (let ((section (magit-insert-section excerpt (dr-excerpt)
                               (insert (propertize (concat text "\n")
                                                   'font-lock-face 'magit-diff-context))
                               excerpt)))
                (funcall add-place (list c (point) section nil))
                (dr--insert-gap)))))))))

(defun dr--visit-text-comment (comment)
  "Show the file which text COMMENT is on in another window, at the comment, keeping the focus
here."
  (let* ((buffer (find-file-noselect (expand-file-name (alist-get 'file comment) (magit-toplevel))))
         (window (let ((display-buffer-overriding-action '((display-buffer-reuse-window
                                                             display-buffer-use-some-window
                                                             display-buffer-pop-up-window)
                                                            (inhibit-same-window . t))))
                   (display-buffer buffer))))
    (with-current-buffer buffer
      (unless dr/file-mode
        (dr/file-mode 1))
      (when-let* ((region (dr--block-region (alist-get 'id comment))))
        (goto-char (car region))
        (forward-line -1)
        (when window
          (set-window-point window (point)))))))

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
      (when (length< excerpts (length comments))
        (dr--insert-gap))
      ;; In the order of the lines in the diff.
      (setq excerpts (sort (nreverse excerpts)
                           (lambda (a b) (< (seq-position lines (car (last (cdr a))))
                                            (seq-position lines (car (last (cdr b))))))))
      (pcase-dolist (`(,c . ,excerpt) excerpts)
        (let ((hunk (dr--insert-excerpt file excerpt)))
          (funcall add-place (list c (point) hunk nil))
          (dr--insert-gap))))))

(defun dr--insert-gap ()
  "Insert the blank line which follows each thread in the review buffer, keeping it apart from the
next heading. The thread goes before it, at the place recorded before this is called."
  (insert "\n"))

(cl-defun dr/review-visit ()
  "Show the commit at point in another window, keeping the focus here, at the comment at point or
the comment below the excerpt at point."
  (interactive)
  (let* ((section (magit-current-section))
         (cid (or (get-text-property (point) 'dr-block)
                  (and (memq (oref section type) '(hunk dr-excerpt))
                       (when-let* ((pos (dr--any-p (oref section start) (oref section end))))
                         (get-text-property pos 'dr-block)))))
         (comment (and cid (gethash cid dr--comments)))
         (commit section))
    (when (and comment (dr--text-comment-p comment))
      (dr--visit-text-comment comment)
      (cl-return-from dr/review-visit))
    (while (and commit (not (eq (oref commit type) 'commit)))
      (setq commit (oref commit parent)))
    (unless (and commit (oref commit value))
      (user-error "Not on a commit"))
    (let ((buffer (dr--show-commit-elsewhere (oref commit value) nil)))
      (when cid
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
    (unless dr/file-mode
      (setq buffer-read-only (not in-comment)))
    (unless (eq in-comment dr--in-comment)
      (setq dr--in-comment in-comment)
      (if in-comment
          (progn
            (setq dr--magit-map (current-local-map))
            (use-local-map dr/comment-map)
            (setq dr--outside-settings (list auto-fill-function evil-auto-indent
                                             electric-indent-mode))
            ;; In a file buffer, the file's mode would indent a comment's lines as code.
            (setq auto-fill-function (if dr/file-mode #'dr--auto-fill #'do-auto-fill))
            (setq-local evil-auto-indent nil)
            (setq-local electric-indent-mode nil))
        (use-local-map dr--magit-map)
        (pcase-let ((`(,fill ,auto-indent ,electric) dr--outside-settings))
          (setq auto-fill-function fill)
          (setq-local evil-auto-indent auto-indent)
          (setq-local electric-indent-mode electric)))
      (evil-normalize-keymaps))))

(defun dr--pre-command ()
  ;; Undoing from a diff line can restore a comment deleted with dd.
  (when (and (memq this-command dr--undo-commands) (not dr/file-mode))
    (setq buffer-read-only nil)))

(defun dr--post-command ()
  (unless (memq evil-state '(insert replace))
    (dr--save))
  (when dr/file-mode
    ;; A save which failed, after the comments were taken out for it.
    (when dr--stripped
      (dr--after-save))
    (when dr--check-blocks
      (setq dr--check-blocks nil)
      (dr--fix-blocks)))
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
         (forward-line -1))
       (dr--skip-gap)))))

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
