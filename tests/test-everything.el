
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
             (evil-normal-state))
      (expect :to-have-buffer-contents "(expressio|n one)")
      (expect :true-in-buffer #'evil-normal-state-p)))
  (it "returns to lispy-state after evil-lispy-insert"
    (-doto (with-test-buffer "(expression| one)"
             (evil-lispy-insert)
             (evil-normal-state))
      (expect :to-have-buffer-contents "|(expression one)")
      (expect :to-be-in-state 'lispy))))
