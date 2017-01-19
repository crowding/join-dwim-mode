
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
                    "line1 line2 #comment1 comment2"))
     (should (looking-back))))

(ert-deftest jd--test-join-beg-comment-3-3 () ""
             (jd--test-with-temp-buffer
                 "line1 #comment1
line2 #|comment2"
                 (join-dwim)
               (should (equal (buffer-string)
                              "line1 line2 #comment1 comment2"))
               (should (looking-back "comment1"))))

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
|line2 # comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1 line2 # comment2"))))

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
|line2 # comment2"
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
|# comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1 #comment1 comment2"))))

(ert-deftest jd--test-join-beg-1-2 () ""
  (jd--test-with-temp-buffer
      "line1
|# comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "line1 # comment2"))))

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
      "# comment1
|# comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "# comment1 comment2"))))

(ert-deftest jd--test-join-beg-0-2 () ""
  (jd--test-with-temp-buffer
      "
|# comment2"
      (join-dwim)
    (should (equal (buffer-string)
                   "# comment2"))))

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

(ert-deftest jd--test-join-beg-buffer () ""
             (jd--test-with-temp-buffer
                 "|test1 #comment1
test2 #comment2"
                 (join-dwim)
               (should (equal (buffer-string)
                              "test1 #comment1
test2 #comment2"))
               (should (looking-at "test1"))))

(ert-deftest jd--test-join-end-buffer () ""
             (jd--test-with-temp-buffer
                 "test1 #comment1
test2 #comment2|"
                 (join-dwim)
               (should (equal (buffer-string)
                              "test1 #comment1
test2 #comment2"))
               (should (looking-back "comment2"))))

(ert-deftest jd--test-join-multiple () ""
             (jd--test-with-temp-buffer
                 "line1 #comment1
line2 #comment2
|line3 #comment3"
                 (join-dwim 2)
               (should (equal (buffer-string)
                              "line1 line2 line3 #comment1 comment2 comment3"))))

(ert-deftest jd--test-join-multiple-down () ""
             (jd--test-with-temp-buffer
                 "line1| #comment1
line2 #comment2
line3 #comment3"
                 (join-dwim 2)
               (should (equal (buffer-string)
                              "line1 line2 line3 #comment1 comment2 comment3"))))

(ert-deftest jd--test-beginning-of-whitespace () ""
    (jd--test-with-temp-buffer
        "    |line1 line2 # comment1 comment2")
  (jd-beginning-of-line-or-comment)
  (should (bobp)))

(ert-deftest jd--test-beginning-of-code () ""
    (jd--test-with-temp-buffer
        "    line1 |line2 # comment1 comment2"
        (jd-beginning-of-line-or-comment)
      (should (looking-at "line1"))))

(ert-deftest jd--test-end-of-code () ""
    (jd--test-with-temp-buffer
        "    line1 |line2 # comment1 comment2"
        (jd-end-of-line-or-comment)
      (should (looking-back "line2"))))

(ert-deftest jd--test-beginning-of-comment () ""
             (jd--test-with-temp-buffer
                 "line1 line2 # comment1 |comment2"
                 (jd-beginning-of-line-or-comment)
               (should (looking-at "comment1"))))

(ert-deftest jd--test-end-of-comment () ""
    (jd--test-with-temp-buffer
        "line1 line2 # comment1 |comment2   "
        (jd-end-of-line-or-comment)
      (should (looking-back "comment2"))))

(ert-deftest jd--test-end-of-whitespace () ""
    (jd--test-with-temp-buffer
        "line1 line2 # comment1 comment2|   "
        (jd-end-of-line-or-comment)
      (should (eobp))))


(ert-deftest jd--test-open-line-in-comment () ""
             (jd--test-with-temp-buffer "line1 #comment1| two
line2"
                 (jd-open-line-dwim)
                 (should (equal (buffer-string)
                                "line1 #comment1
#two
line2"))
                 (should (looking-back "comment1"))))

(ert-deftest jd--test-open-line-under-comment () ""
             (jd--test-with-temp-buffer "line1 |two #comment1
#comment2"
                 (jd-open-line-dwim)
                 (should (equal (buffer-string)
                              "line1 #comment1
two
#comment2"))
                 (should (looking-back "line1 "))))

(ert-deftest jd--test-open-line-under-comment-2 () ""
             (jd--test-with-temp-buffer "line1| two #comment1
#comment2"
                 (jd-open-line-dwim)
               (should (equal (buffer-string)
                              "line1 #comment1
two
#comment2"))))

(ert-deftest jd--test-open-line-after-code () ""
             (jd--test-with-temp-buffer
                 "line1 two| #comment1
#comment2"
                 (jd-open-line-dwim)
                 (should (equal (buffer-string)
                                "line1 two
#comment1
#comment2"))
                 (should (looking-back "two"))))

(ert-deftest jd--test-open-line-before-code () ""
             (jd--test-with-temp-buffer
                 "|line1 two #comment1
#comment2"
                 (jd-open-line-dwim)
               (should (equal (buffer-string)
                              "
line1 two #comment1
#comment2"))
               (should (eolp))))

(ert-deftest jd--test-newline-before-code () ""
             (jd--test-with-temp-buffer
                 "|line1 #comment1
#comment2"
                 (jd-newline-dwim)
               (should (equal (buffer-string)
                              "
line1 #comment1
#comment2"))
               (should (looking-at "line1"))))

(ert-deftest jd--test-newline-under-comment () ""
             (jd--test-with-temp-buffer "line1| two #comment1
#comment2"
                 (jd-newline-dwim)
               (should (equal (buffer-string)
                              "line1 #comment1
two
#comment2"))
               (should
                (looking-at "two"))))

(ert-deftest jd--test-newline-in-comment () ""
             (jd--test-with-temp-buffer "line1 two #comment1 |two
#comment2"
                 (jd-newline-dwim)
               (should (equal (buffer-string)
                              "line1 two #comment1
#two
#comment2"))))

;; do we want a spill-over option? say no for now.
