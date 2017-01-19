(eval-when-compile (require 'pcase)) ; for `pcase-let'

(defvar join-dwim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap join-line] 'join-dwim)
    (define-key map [remap delete-indentation] 'join-dwim)
    (define-key map [remap move-beginning-of-line] 'jd-beginning-of-line-or-comment)
    (define-key map [remap move-end-of-line] 'jd-end-of-line-or-comment)
    (define-key map [remap open-line] 'jd-open-line-dwim)
    (define-key map [remap indent-new-comment-line] 'jd-newline-dwim)
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
                               (jd--comment-markers)))
              (`(,code-start-2 ,code-end-2 ,comment-mark-2 ,comment-start-2 ,comment-end-2)
               (save-excursion (if (>= direction 0) (forward-line))
                               (jd--comment-markers))))
    (unless (= code-start-1 code-start-2)
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
         (goto-char comment-end-1)
         (jd--insert-and-fixup-whitespace comment-2-ex)
         (goto-char code-end-1)
         (jd--insert-and-fixup-whitespace code-2-ex)
         (if in-comment
             (goto-char comment-end-1)
           (goto-char code-end-1)))))))

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

(defun jd--end-of-nonwhitespace ()
  (interactive)
  (end-of-line)
  (skip-syntax-backward " ")
  (point))

(defun jd--comment-markers (&optional p)
  " returns markers determined from the current line in the following order.
   < Beginning of code
   < end of code
   > start of comment marker
   > beginning of comment
   < end of comment"
  (setq p (or p (point)))
  (let ((eol (copy-marker (save-excursion (jd--end-of-nonwhitespace))))
        (boc (save-excursion (back-to-indentation) (point-marker))))
    (cond ((save-excursion (end-of-line) (jd--in-line-comment-p)) ;; line has comment?
           (let ((eoc nil) (marker nil) (boa nil) (state nil))
             (save-excursion
               (beginning-of-line)
               (setq state (jd--parse-state))
               (parse-partial-sexp (point) eol nil nil state t) ;to a comment
               (backward-char)
               (setq marker (copy-marker (point) t))
               (skip-syntax-backward " ")
               (setq eoc (point-marker))
               (goto-char marker)
               (skip-syntax-forward "<")
               (skip-syntax-forward " ")
               (while (eq ?  (char-syntax (char-after))) (forward-char))
               (setq begin (point-marker)))
             (list boc eoc marker begin eol)))
          (t (save-excursion
               (end-of-line)
               (list boc (copy-marker eol nil) eol eol (copy-marker eol nil)))))))

(defun jd--closer-to-beginning-p ()
  "True if the cursor is closer or equal to the beginning of the
code line or line comment than the end."
  (pcase-let* ((pt (point))
               (`(,code-start ,code-end ,comment-mark ,comment-start ,comment-end)
                (jd--comment-markers))
               (in-comment (> pt comment-mark))
               (distance-to-beginning
                (abs (- pt (if in-comment comment-start code-start))))
               (distance-to-end
                (abs (- pt (if in-comment comment-end code-end)))))
    (>= distance-to-end distance-to-beginning)))

(defun jd-beginning-of-line-or-comment ()
  (interactive)
  (pcase-let ((`(,code-start ,code-end ,comment-mark ,comment-start ,comment-end)
               (jd--comment-markers)))
    (skip-backward-along
     (list comment-start code-start (line-beginning-position)))))

(defun jd-end-of-line-or-comment ()
  (interactive)
  (pcase-let ((`(,code-start ,code-end ,comment-mark ,comment-start ,comment-end)
               (save-excursion (end-of-line)
                               (jd--comment-markers))))
    (skip-forward-along (list code-end comment-end (line-end-position)))))

(defun skip-forward-along (xs)
  (when xs
    (let ((x (car xs)))
      (if (> x (point))
          (goto-char x)
        (skip-forward-along (cdr xs))))))

(defun skip-backward-along (xs)
  (when xs
    (let ((x (car xs)))
      (if (< x (point))
          (goto-char x)
        (skip-backward-along (cdr xs))))))

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

(defun jd-open-line-dwim (&optional times)
  (interactive "p")
  (let ((times (or times 1)))
    (dotimes (i times)
      (if (jd--in-line-comment-p)
          (jd--open-line-in-comment)
        (jd--open-line-under-comment)))))

(defun jd--open-line-in-comment ()
  (let ((mark (point-marker)))
    (indent-new-comment-line)
    (goto-char mark)))

(defun jd--open-line-under-comment ()
  (pcase-let* ((`(,code-start ,code-end ,jd-comment-mark ,jd-comment-start ,jd-comment-end)
                (jd--comment-markers))
               (mark (point-marker)))
    (cond
     ((and (> jd-comment-end jd-comment-mark) ;line has comment
           (> code-end mark)
           (> mark code-start)) ;and has code to break to the next line
      (let ((rgn (delete-and-extract-region jd-comment-mark jd-comment-end)))
        (newline :interactive t)
        (end-of-line)
        (delete-horizontal-space)
        (goto-char mark)
        (insert rgn)
        (goto-char mark)
        (just-one-space)
        )
      )
     (t ; otherwise just newline
      (newline :interactive t)
      (goto-char mark)))))

(defun jd-newline-dwim (&optional arg)
  (interactive "p")
  (let ((arg (or arg 1)))
    (dotimes (i arg)
      (cond
       ((jd--in-line-comment-p)
        (indent-new-comment-line))
       (t                              ; not in a comment
        (jd--newline-under-comment))))))

(defun jd--newline-under-comment ()
  (pcase-let* ((pt (point))
               (`(,code-start ,code-end ,comment-mark ,jd-comment-start ,jd-comment-end)
                (jd--comment-markers)))
    (cond
     ((and (> jd-comment-end comment-mark) ; line has comment
           (> pt code-start))
      (let ((mark (point-marker))
            (rgn (delete-and-extract-region code-end jd-comment-end)))
        (newline :interactive t)
        (save-excursion (goto-char mark)
                        (insert rgn)
                        (goto-char mark)
                        (fixup-whitespace))
        (save-excursion (end-of-line) (delete-horizontal-space))))
     (t
      (newline :interactive t)))))

(provide 'join-dwim)
