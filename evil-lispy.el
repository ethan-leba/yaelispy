(require 'evil)
(require 'lispy)

;; ——— Mode/State definition ——————————————————————————————————————————————————

;;;###autoload
(define-minor-mode evil-lispy-mode
  "Context sensitive paredit"
  :lighter " eLY"
  :keymap (make-sparse-keymap)
  :after-hook (evil-normal-state))


;; Evil sexp editing state
(evil-define-state
  lispy "lispy operation state"
  :tag " <S> "
  :cursor ("orange" box)
  :suppress-keymap t

  ;; disable automatically going into visual mode...
  (if (evil-lispy-state-p)
      (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)
    (add-hook 'activate-mark-hook 'evil-visual-activate-hook nil t)))


;; ——— Utilities ————————————————————————————————————————————————————————---—--

(defmacro evil-lispy--bind (&rest code)
  "Helper to make an bindable command"
  `(lambda ()
    (interactive)
    ,@code))

(defun evil-lispy--insert-after (fun)
  "Helper to make an bindable command"
  (lambda ()
    (interactive)
    (funcall fun)
    (evil-insert-state)))

(defun evil-lispy--jump-back (fun)
  "Helper to make an bindable command"
  `(lambda ()
     (interactive)
     (funcall fun)
     (lispy-backward 1)))

(defun evil-lispy-alter-sexp-left ()
  "Move bound of sexp left"
  (interactive)
  (if (looking-at lispy-left)
      (lispy-slurp 1)
    (lispy-barf 1)))

(defun evil-lispy-alter-sexp-right ()
  "Move bound of sexp right"
  (interactive)
  (if (looking-at lispy-left)
      (lispy-barf 1)
    (lispy-slurp 1)))

(defun evil-lispy-insert ()
  "Move bound of sexp left"
  (interactive)
  (when (looking-at lispy-right)
    (backward-char))
  (evil-insert 0))

(defun evil-lispy-append ()
  "Move bound of sexp left"
  (interactive)
  (when (looking-at lispy-right)
    (backward-char))
  (evil-append 0))

;; ——— Evil standard keymap overrides —————————————————————————————————————————

(let ((map evil-lispy-mode-map))
  ;; Entering lispy state
  (evil-define-key 'normal map (kbd "(")
    (evil-lispy--bind
     (evil-lispy-state)
     (evil-forward-char 1 nil t)
     (lispy-backward 1)))
  (evil-define-key 'normal map (kbd ")")
    (evil-lispy--bind
     (evil-lispy-state)
     (evil-backward-char 1 nil t)
     (lispy-forward 1)))
  (evil-define-key 'insert map [escape]
    (evil-lispy--bind
     (evil-lispy-state)
     (lispy-backward 1)))
  map)


;; ——— Lispy state binds ——————————————————————————————————————————————————————

(let ((map evil-lispy-state-map))
  ;; Exiting state
  (define-key map (kbd "C-g")
    (evil-lispy--bind
     (deactivate-mark)
     (evil-normal-state)))

  (define-key map [escape] (kbd "C-g"))

  (define-key map "i" 'evil-lispy-insert)

  (define-key map "a" 'evil-lispy-append)
  (define-key map "A" 'lispy-beginning-of-defun)
  ;; (define-key map (kbd "SPC") (evil-lispy--bind
  ;;                              (lispy-space) (evil-insert-state)))
  (define-key map (kbd "RET") (evil-lispy--bind
                               (lispy-newline-and-indent) (evil-insert-state)))

  ;; Navigation
  (define-key map (kbd "(") #'lispy-parens)
  (define-key map (kbd ")") 'lispy-out-forward)
  (define-key map (kbd "C-o") 'evil-jump-backward) ; need to go to normal mode if out of special
  (define-key map (kbd "C-i") 'evil-jump-forward) ; need to go to normal mode if out of special
  (define-key map (kbd "C-e") 'lispy-move-end-of-line)

  (define-key map "h" 'lispy-backward)
  (define-key map "j" 'lispy-down)
  (define-key map "k" 'lispy-up)
  (define-key map "l" 'lispy-forward)
  (define-key map "f" 'lispy-flow)
  (define-key map "o" 'lispy-different)
  (define-key map "gd" 'lispy-follow)
  (define-key map "G" 'lispy-goto)
  (define-key map "q" 'lispy-ace-paren)
  (define-key map "Q" 'lispy-ace-char)

  ;; Paredit transformations
  (define-key map ">" 'evil-lispy-alter-sexp-right)
  (define-key map "<" 'evil-lispy-alter-sexp-left)
  (define-key map "/" 'lispy-splice)
  (lispy-define-key map "r" 'lispy-raise t)
  (define-key map "R" 'lispy-raise-some)
  (define-key map (kbd "gJ") 'lispy-split)
  (define-key map "J" 'lispy-join)
  (lispy-define-key map "C" 'lispy-convolute t)
  (lispy-define-key map "C-k" 'lispy-move-up t)
  (lispy-define-key map "C-j" 'lispy-move-down t)
  (lispy-define-key map "O" 'lispy-oneline t)
  (lispy-define-key map "M" 'lispy-multiline t)
  (define-key map (kbd ";") 'lispy-comment)
  (define-key map "c" 'lispy-clone)
  (define-key map "t" 'lispy-teleport)

  ;; Kill related
  ;; (define-key map (kbd "DEL")
  ;;   (evil-lispy--jump-back (lispy-delete-backward 0)))
  (define-key map "d" (evil-lispy--jump-back (lispy-kill-at-point)))
  (define-key map "S" (evil-lispy--bind
                       (lispy-kill-at-point) (evil-insert 0)))
  (define-key map "p" 'lispy-yank)
  (define-key map "y" 'lispy-new-copy)


  ;; Marking
  (define-key map "s" 'lispy-ace-symbol)
  (define-key map "gs" 'lispy-ace-symbol-replace)
  (define-key map "v" 'lispy-mark-symbol)
  (define-key map "V" 'lispy-mark-list)


  ;; Misc
  (define-key map (kbd "C-1") 'lispy-describe-inline)
  (define-key map (kbd "C-2") 'lispy-arglist-inline)
  (define-key map "u" 'lispy-undo)
  (define-key map "e" 'lispy-eval)
  (define-key map "E" 'lispy-eval-and-insert)
  (define-key map "K" 'lispy-describe)

  (define-key map "gq" 'lispy-normalize)
  (define-key map "=" 'lispy-tab)
  (define-key map (kbd "TAB") 'lispy-shifttab)
  (lispy-define-key map "z" 'lispy-view t)

  ;; Digit argument
  (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
        (number-sequence 0 9)))



(evil-add-command-properties 'lispy-out-forward :jump t)
(evil-add-command-properties 'lispy-out-backward :jump t)
(evil-add-command-properties 'lispy-follow :jump t)


(provide 'evil-lispy)
