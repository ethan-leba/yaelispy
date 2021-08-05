
(describe "with-test-buffer macro"
  (it "inserts buffer contents and returns them"
    (expect (with-test-buffer "hello world|")
            :to-have-buffer-contents "hello world|"))
  (it "allows calling code in the given buffer"
    (expect (with-test-buffer "|world"
              ;; move cursor to end of line
              (evil-append-line 1))
            :to-have-buffer-contents "world|")))


(describe "enter lispy-mode at edges of the current expression"
  (it "before an expression"
    (expect (with-test-buffer "(an expression| here)"
              (evil-normal-state)
              (ot--keyboard-input
               (ot--type "(")))
            :to-have-buffer-contents "|(an expression here)"))

  (xit "after an expression"
    (expect (with-test-buffer "(an expression| here)"
              (evil-normal-state)
              (ot--keyboard-input
               (ot--type ")")))
            :to-have-buffer-contents "(an expression here)|")))

(describe "exiting insert-mode"
  (it "acts normally when not using evil-lispy-insert"
    (-doto (with-test-buffer "(expression| one)"
             (evil-insert-state)
             (ot--keyboard-input (ot--type (kbd "ESC"))))
      (expect :true-in-buffer #'evil-normal-state-p)))
  (it "returns to lispy-state after evil-lispy-insert"
    (-doto (with-test-buffer "|(expression one)"
             (evil-lispy-insert)
             (ot--keyboard-input (ot--type (kbd "ESC"))))
      (expect :true-in-buffer #'evil-lispy-state-p))))

(describe "insert and append"
  (it "selects beginning/end of list"
    (-doto (with-test-buffer "|(expression one)"
             (evil-lispy-insert))
      (expect :to-have-buffer-contents "(|expression one)"))
    (-doto (with-test-buffer "`|(expression one)"
             (evil-lispy-insert))
      (expect :to-have-buffer-contents "`(|expression one)"))
    (-doto (with-test-buffer "|(expression one)"
             (evil-lispy-append))
      (expect :to-have-buffer-contents "(expression one|)")))
  (it "selects beginning/end of region when active"
    (-doto (with-test-buffer "|(expression one)"
             (lispy-mark-car)
             (evil-lispy-insert))
      (expect :to-have-buffer-contents "(|expression one)"))
    (-doto (with-test-buffer "|(expression one)"
             (lispy-mark-car)
             (evil-lispy-append))
      (expect :to-have-buffer-contents "(expression| one)"))))

(describe "transient normal mode"
  (it "will go back to lispy state after an edit operation"
    (-doto (with-test-buffer "|(expression one)"
             (evil-lispy-enter-transient-normal-state)
             (ot--keyboard-input
              (ot--type "x")))
      (expect :true-in-buffer #'evil-lispy-state-p)))
  (it "deactivates mark"
    (-doto (with-test-buffer "|(expression one)"
             (lispy-mark-car)
             (evil-lispy-enter-transient-normal-state))
      (expect :to-have-buffer-contents "(expression| one)")))
  (it "won't go back to lispy state after an operation not in the list"
    (-doto (with-test-buffer "|(expression one)"
             (evil-lispy-enter-transient-normal-state)
             (ot--keyboard-input
              (ot--type "l")))
      (expect :true-in-buffer #'evil-normal-state-p)))
  (it "won't go back to lispy state after lispy state and then back to normal state"
    (-doto (with-test-buffer "|(expression one)"
             (evil-lispy-enter-transient-normal-state)
             (evil-lispy-state)
             (ot--keyboard-input
              ;; Enter normal mode via user command
              (ot--meta-x-command "evil-normal-state")
              (ot--type "x")))
      (expect :true-in-buffer #'evil-normal-state-p))))
