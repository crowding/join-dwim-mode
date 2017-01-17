(eval-when-compile (require 'pcase)) ; for `pcase-let'

(defvar join-dwim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap join-line] 'join-dwim)
    (define-key map [remap delete-indentation] 'join-dwim)
    map))

(define-minor-mode join-dwim-mode
  "Join and split DWIM.

   Remaps join-line to join-dwim.
   Interactively with no argument, this command toggles the mode.
   A positive prefix argument enables the mode, any other prefix
   argument disables it.  From Lisp, argument omitted or nil
   enables the mode, `toggle' toggles the state.
   Configuration variables:

  This remaps the keys bound to join-lines. The mode map is:
 \\{join-dwim-mode-map}"

  :keymap 'join-dwim-mode-map

  (join-dwim-enable-if))

(defun join-dwim-enable-if ()
  (if join-dwim-mode
      (join-dwim-enable)
    (join-dwim-disable)))

(defun join-dwim-enable ())

(defun join-dwim-disable ())

(defun join-dwim (&optional arg)
  "A DWIM version of join-line. Joins the current line
 either to the line preceding or the line following, whichever is
 closer to point. Tries to keep code and line comments separate.
 If region is active, joins together the lines in the the region."
  (interactive "p")
  (if (null arg) (setq arg 1))
  (let ((direction (if (jd--closer-to-beginning-p) -1 1)))
    (dotimes (x arg nil)
      (jd--join-code-lines direction))))

(defun jd--join-code-lines (direction)
  "Like `join-line', but respects comments, joining code and comment
   separately and skipping the comment marker.
   Negative values join to preceding lines and
   positive values join to following lines."
  (pcase-let ((`(,code-start-1 ,code-end-1 ,comment-mark-1 ,comment-start-1 ,comment-end-1)
               (save-excursion (if (< direction 0) (forward-line -1))
                               (end-of-line)
                               (jd--comment-markers)))
              (`(,code-start-2 ,code-end-2 ,comment-mark-2 ,comment-start-2 ,comment-end-2)
               (save-excursion (if (>= direction 0) (forward-line))
                               (end-of-line)
                               (jd--comment-markers))))
    (unwind-protect
        (let* ((has-comment-1 (/= comment-mark-1
                                  comment-start-1))
               (has-code-1 (/= code-start-1 code-end-1))
               (has-comment-2 (/= comment-start-2 comment-end-2))
               (has-code-2 (/= code-start-2 code-end-2))
               (in-comment
                (if (< direction 0)
                    (< comment-mark-2 (point))
                  (< comment-mark-1 (point))))
               (code-2-ex
                (delete-and-extract-region code-start-2 code-end-2))
               (comment-2-ex
                (delete-and-extract-region
                 (if has-comment-1 comment-start-2 comment-mark-2)
                 comment-end-2)))
          (delete-region comment-end-1 comment-end-2)
          (goto-char code-end-1)
          (jd--insert-and-fixup-whitespace code-2-ex)
          (goto-char comment-end-1)
          (jd--insert-and-fixup-whitespace comment-2-ex)))))

(defun jd--insert-and-fixup-whitespace (string)
  (let ((before (copy-marker (point) nil))
        (after (copy-marker (point) t)))
    (unless (string-blank-p string)
      (insert string)
      (goto-char after)
      (unless (blank-after)
        (fixup-whitespace))
      (goto-char before)
      (unless (blank-before)
        (fixup-whitespace)))))

(defun blank-after ()
  (string-blank-p (string (or (char-after) 32))))

(defun blank-before ()
  (string-blank-p (string (or (char-after) 32))))

(defun jd--comment-markers (&optional p)
  " returns markers determined from the current line in the following order.
   < Beginning of code
   < end of code
   < start of comment marker
   > beginning of comment
   > end of comment"
  (setq p (or p (point)))
  (let ((eol (copy-marker (line-end-position) t))
        (boc (save-excursion (back-to-indentation) (point-marker))))
    (cond ((save-excursion (end-of-line) (jd--in-line-comment-p)) ;; line has comment?
           (let ((eoc nil) (marker nil) (boa nil) (state nil))
             (save-excursion
               (beginning-of-line)
               (setq state (jd--parse-state))
               (parse-partial-sexp (point) eol nil nil state t) ;to a comment
               (backward-char)
               (setq eoc (point-marker))
               (setq marker (copy-marker (point) t))
               (while (eq ?< (char-syntax (char-after))) (forward-char))
               (setq begin (point-marker)))
             (list boc eoc marker begin eol)))
          (t (save-excursion
               (end-of-line)
               (list boc eol eol (copy-marker eol t) (copy-marker eol t)))))))

(defun jd--closer-to-beginning-p ()
  "True if the cursor is closer to the beginning of the line than
  the end, excluding whitespace and line-ending comments."
  (let* ((pt (point))
         (distance-to-beginning
          (save-excursion
            (back-to-indentation)
            (abs (- (point) pt))))
         (distance-to-end
          (save-excursion
            (jd--end-of-line-ignoring-comment-whitespace)
            (abs (- (point) pt)))))
    (>= distance-to-end distance-to-beginning)))

(defun jd--end-of-line-ignoring-comment-whitespace ()
  (interactive)
  (when (comment-search-forward (line-end-position) t)
    (goto-char (match-beginning 0))
    (skip-syntax-backward " " (line-beginning-position))))

(defun jd--beginning-of-line-or-comment ()
  (interactive)
  (cond ( (comment-search-backward (line-beginning-position))
          (skip-syntax-forward " " (line-ending-position))
          (if (eq (point) (line-ending-position))
              (skip-syntax-backward " " (line-ending-position))))
        ( t
          (back-to-indentation))))

(defun jd--parse-state (&optional p)
  "return the parse state at point"
  (setq p (or p (point)))
  (save-excursion (goto-char p)
                  (beginning-of-defun)
                  (parse-partial-sexp (point) p)))

(defun jd--in-line-comment-p (&optional p)
  "return TRUE if point is in a line-ending comment"
  (let* ((p (or p (point)))
         (state
          (save-excursion (goto-char p)
                          (beginning-of-defun) (parse-partial-sexp (point) p))))
    (and (nth 4 state) t)))

;; (defun jd--open-line-near-point
;;   "Open a line before or after the current line, whichever is
;; closer, autoindent, and place the cursor in the new blank line."
;;   (interactive "p")
;; )

(provide 'join-dwim)
