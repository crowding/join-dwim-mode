
(require 'join-dwim)

(defmacro jd--test-with-temp-buffer (initial &rest forms)
  "Setup a new buffer, then run FORMS.
First, INITFORM are run in the newly created buffer.
Then `join-dwim-mode' is turned on.  Then INITIAL is
inserted (it is expected to evaluate to string).  If INITIAL
contains | put point there as the initial position (the character
is then removed).  If it contains M, put mark there (the
character is then removed).
Finally, FORMS are run."
  (declare (indent 2)
           (debug (form form body)))
  `(save-window-excursion
     (let ((case-fold-search nil))
       (with-temp-buffer
         (set-input-method nil)
         (shell-script-mode)
         (join-dwim-mode 1)
         (pop-to-buffer (current-buffer))
         (insert ,initial)
         (goto-char (point-min))
         (when (search-forward "M" nil t)
           (delete-char -1)
           (set-mark (point))
           (activate-mark))
         (goto-char (point-min))
         (when (search-forward "|" nil t)
           (delete-char -1))
         ,@forms))))

(ert-deftest jd--test-join-beg-3-3 () ""
    (jd--test-with-temp-buffer
     "line1 #comment1
|line2 #comment2"
     (join-dwim)
     (should (equal (buffer-string)
                    "line1 line2 #comment1 comment2"))))

(ert-deftest jd--test-join-beg-3-3 () ""
    (jd--test-with-temp-buffer
     "line1 #comment1
|line2 #comment2"
     (join-dwim)
     (should (equal (buffer-string)
                    "line1 line2 #comment1 comment2"))))

(ert-deftest jd--test-join-beg-3-2 () ""
  (jd--test-with-temp-buffer
      "line1
|line2 #comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1 line2 #comment2"))))

(ert-deftest jd--test-join-beg-3-1 () ""
  (jd--test-with-temp-buffer
      "line1 #comment1
|line2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1 line2 #comment1"))))


(ert-deftest jd--test-join-beg-3-0 () ""
  (jd--test-with-temp-buffer
      "line1
|line2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1 line2"))))

(ert-deftest jd--test-join-beg-2-3 () ""
  (jd--test-with-temp-buffer
      "#comment1
|line2 #comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line2 #comment1 comment2"))))


(ert-deftest jd--test-join-beg-2-2 () ""
  (jd--test-with-temp-buffer
      "
|line2 #comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line2 #comment2"))))


(ert-deftest jd--test-join-beg-2-1 () ""
  (jd--test-with-temp-buffer
      "#comment1
|line2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line2 #comment1"))))

(ert-deftest jd--test-join-beg-2-0 () ""
  (jd--test-with-temp-buffer
      "
|line2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line2"))))

(ert-deftest jd--test-join-beg-1-3 () ""
  (jd--test-with-temp-buffer
      "line1 #comment1
|#comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1 #comment1 comment2"))))

(ert-deftest jd--test-join-beg-1-2 () ""
  (jd--test-with-temp-buffer
      "line1
|#comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1 #comment2"))))

(ert-deftest jd--test-join-beg-1-1 () ""
  (jd--test-with-temp-buffer
      "line1 #comment1
|"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1 #comment1"))))

(ert-deftest jd--test-join-beg-1-0 () ""
  (jd--test-with-temp-buffer
      "line1
|"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1"))))

(ert-deftest jd--test-join-beg-0-3 () ""
  (jd--test-with-temp-buffer
      "#comment1
|#comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "#comment1 comment2"))))

(ert-deftest jd--test-join-beg-0-2 () ""
  (jd--test-with-temp-buffer
      "
|#comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "#comment2"))))

(ert-deftest jd--test-join-beg-0-1 () ""
  (jd--test-with-temp-buffer
      "#comment1
|"
      (join-dwim)
    (should (equal (buffer-string)
                   "#comment1"))))

(ert-deftest jd--test-join-beg-0-0 () ""
  (jd--test-with-temp-buffer
      "
|"
      (join-dwim)
    (should (equal (buffer-string)
                   ""))))

