(require 'evil-lispy)
(require 'dash)
(require 'cl)
(require 'buttercup)
(require 's)

(defun buffer-status-as-string ()
  (let ((p (point))
        (m (when mark-active (mark))))
    (goto-char p)
    (insert "|")

    ;; show mark as well (other side of selection, if any)
    (when m
      (message "oh hi marc")
      (goto-char m)
      (insert "~")))

  (buffer-substring-no-properties (point-min)
                                  (point-max)))

(defun insert-one-or-many-lines (input)
  (cond ((stringp input)
         (insert input))
        (t ;; it's a list
         (--each input (insert it "\n"))
         ;; remove last extra newline
         (backward-delete-char 1))))

(defmacro with-test-buffer (contents &rest test-forms)
  "This awesome macro is adapted (borrowed) from
  https://github.com/abo-abo/lispy/blob/master/lispy-test.el#L15"
  (declare (indent 1))
  `(progn
     (-when-let (b (get-buffer "evil-lispy-test-buffer"))
       (kill-buffer b))
     (let ((temp-buffer (get-buffer-create "evil-lispy-test-buffer")))
       (save-window-excursion
         (switch-to-buffer temp-buffer)
         (emacs-lisp-mode)
         (evil-mode)
         (evil-lispy-mode)
         (evil-lispy-state)

         (insert-one-or-many-lines ,contents)

         (evil-goto-first-line)
         (when (search-forward "|")
           (backward-delete-char 1))

         ,@test-forms

         temp-buffer))))

(buttercup-define-matcher :to-have-buffer-contents (test-buffer
                                                    expected-contents)
  (setq test-buffer (funcall test-buffer))
  (setq expected-contents (funcall expected-contents))
  (with-current-buffer test-buffer
    (let ((contents (buffer-status-as-string)))
      (if (equal contents expected-contents)
          t
        `(nil . ,(format "Expected '%s' to equal '%s'."
                         contents
                         expected-contents))))))

(buttercup-define-matcher :true-in-buffer (test-buffer func)
  (setq test-buffer (funcall test-buffer))
  (with-current-buffer test-buffer
    (funcall (funcall func))))

;; these are borrowed from omnisharp-emacs
;;
(defun ot--keyboard-input (&rest text-vectors)
  "Simulates typing. Can be used to do interactive input, but
detecting situations in the middle of input is impossible.
Be careful: weird errors may happen if you try to call functions in the middle
of this function. Only use text-vectors."
  (condition-case error
      (execute-kbd-macro (reduce 'vconcat text-vectors))
    (error (print (format "ot--keyboard-input error: %s" error)))))

(defun ot--meta-x-command (command)
  (vconcat
   (ot--press-key "M-x")
   (ot--type command)
   (ot--press-key "RET")))

(defun ot--type (text)
  (string-to-vector text))

(defun ot--press-key (key-or-chord)
  (edmacro-parse-keys key-or-chord))
