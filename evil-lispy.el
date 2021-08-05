;;; evil-lispy.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ethan Leba
;;
;; Author: Ethan Leba <https://github.com/ethanleba>
;; Maintainer: Ethan Leba <ethanleba5@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/ethanleba/evil-lispy
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; YAE-Lispy is Yet-Another-Evil Lispy variant that aims to make lispy familiar
;; and intuitive to the Vim user while retaining the full power of Lispy's
;; structural editing capabilities.
;;
;; YAE-Lispy differs from it's peers in the following goals:
;; 1. Vimify Lispy as much as possible, leveraging the Vimmer's muscle memory and minimizing
;; the need to learn/use Emacs binding with lispy.
;; 2. Make Lispy the primary state, while allowing for transient forays into Normal state.
;;
;;  Description
;;
;;; Code:

(require 'evil)
(require 'lispy)

;;* Variables
(defvar evil-lispy-normal-exit-commands
  '(lispyville-delete
    lispyville-delete-line
    lispyville-delete-char-or-splice
    evil-delete-char
    evil-replace)
  "The list of Normal state commands that should trigger re-entry into Lispy state.")

(defvar evil-lispy--command-wrappers
  '((:insert . evil-lispy--insert-after)
    (:jump . evil-lispy--jump-back)
    (:hybrid . evil-lispy--insert-nonspecial)))
;;* Mode/State definition

;;;###autoload
(define-minor-mode evil-lispy-mode
  "Context sensitive paredit"
  :lighter " eLY"
  :keymap (make-sparse-keymap)
  :init-value nil
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

;;* Utilities
(defmacro evil-lispy--bind (&rest code)
  "Helper to make an bindable command. "
  `(lambda ()
     (interactive)
     ,@code))

(defun evil-lispy--insert-after (fun)
  "Helper to make an bindable command"
  `(lambda ()
     ,(format "Call `%s', and then enter insert mode.\n\n%s"
              (symbol-name fun) (documentation fun))
     (interactive) (call-interactively #',fun) (evil-insert-state)))

(defun evil-lispy--jump-back (fun)
  "Helper to make an bindable command"
  `(lambda ()
     ,(format "Call `%s', and then jump to the left-most paren.\n\n%s"
              (symbol-name fun) (documentation fun))
     (interactive) (call-interactively #',fun) (lispy-down 1)))

(defun evil-lispy--insert-nonspecial (fun)
  "Helper to make an bindable command"
  `(lambda ()
     ,(format "Call `%s', and then jump to the left-most paren.\n\n%s"
              (symbol-name fun) (documentation fun))
     (interactive)
     (call-interactively #',fun)
     (unless (or (region-active-p)
                 (lispy-left-p)
                 (lispy-right-p)
                 (and (lispy-bolp)
                      (or (looking-at lispy-outline-header)
                          (looking-at lispy-outline))))
       (evil-insert-state))))

(defun evil-lispy-define-key (keymap key def type)
  "Forward to (`define-key' KEYMAP KEY FUNC).
FUNC is obtained from (`lispy--insert-or-call' DEF PLIST)."
  (declare (indent 3))
  (require 'eldoc)
  (let* ((lamb (alist-get type evil-lispy--command-wrappers))
         (func (defalias (intern (concat "yae-" (symbol-name def) "-and-" (substring (symbol-name type) 1)))
                 (funcall lamb def))))
    (eldoc-add-command func)
    (define-key keymap (kbd key) func)))

;;* Evil-Lispy commands
(defun evil-lispy-enter-transient-normal-state ()
  "Execute the next command in Normal state."
  (interactive)
  (let ((marker (point-marker)))
    (evil-normal-state)
    (evil-delay `(or (memq this-command `,evil-lispy-normal-exit-commands)
                     (evil-lispy-state-p))
        `(unless (evil-lispy-state-p)
           (with-current-buffer ,(current-buffer)
             (when (evil-normal-state-p)
               ;; TODO: disable inc. search highlighting (hope this doesn't affect anything else!)
               ;; (evil-force-normal-state)
               )
             (evil-lispy-state)
             (goto-char (marker-position ,marker))))
      'post-command-hook)
    (deactivate-mark t)))

(defun evil-lispy-enter-transient-insert-state ()
  "Execute the next command in Normal state."
  (interactive)
  (evil-insert-state)
  (evil-delay `(or (not (evil-insert-state-p))
                   (evil-lispy-state-p))
      `(with-current-buffer ,(current-buffer)
         (unless (evil-lispy-state-p)
           (evil-lispy-state)
           (lispy-mark-symbol))
         ;; FIXME: This marking behavior is not sane.
         )
    'post-command-hook)
  (deactivate-mark t))

(defun evil-lispy-alter-sexp-right ()
  "Move bound of sexp right"
  (interactive)
  (if (looking-at lispy-left)
      (lispy-barf 1)
    (lispy-slurp 1)))

(defun evil-lispy-alter-sexp-left ()
  "Move bound of sexp left"
  (interactive)
  (if (looking-at lispy-left)
      (lispy-slurp 1)
    (lispy-barf 1)))

(defun evil-lispy-bounds ()
  (interactive)
  (if (not mark-active)
      ;; TODO: Do this with just lispy commands?
      (save-excursion
        (when (lispy-right-p)
          (lispy-different))
        (cons (1+ (point))
              (progn (lispy-different)
                     (1- (point)))))
    (car (region-bounds))))

(defun evil-lispy-open-below ()
  (interactive)
  (lispy-newline-and-indent)
  (previous-line)
  (evil-lispy-enter-transient-insert-state))

(defun evil-lispy-open-above ()
  (interactive)
  (lispy-different)
  (lispy-newline-and-indent)
  (evil-lispy-enter-transient-insert-state))

(defun evil-lispy-insert ()
  "Move bound of sexp left"
  (interactive)
  (goto-char (car (evil-lispy-bounds)))
  (evil-lispy-enter-transient-insert-state))

(defun evil-lispy-append ()
  "Move bound of sexp left"
  (interactive)
  (goto-char (cdr (evil-lispy-bounds)))
  (evil-lispy-enter-transient-insert-state))

(defun evil-lispy-repeat ()
  "Repeat last command with last prefix arg."
  (interactive)
  (unless (equal last-command 'evil-lispy-repeat)
    (setq lispy-repeat--command last-command)
    (setq lispy-repeat--prefix-arg
          (or last-prefix-arg 1)))
  (setq current-prefix-arg lispy-repeat--prefix-arg)
  (call-interactively lispy-repeat--command))



;;* Evil standard keymap overrides
(let ((map evil-lispy-mode-map))
  ;; Entering lispy state
  (evil-define-key 'normal evil-lispy-mode-map (kbd "(")
    (evil-lispy--bind
     (evil-lispy-state)
     (evil-forward-char 1 nil t)
     (lispy-backward 1)))
  (evil-define-key 'normal map (kbd ")")
    (evil-lispy--bind
     (evil-lispy-state)
     (evil-backward-char 1 nil t)
     (lispy-forward 1)))
  (evil-define-key 'insert map (kbd "SPC") #'lispy-space))


;;* Lispy state binds
(let ((map evil-lispy-state-map))
  ;; Exiting state
  (define-key map (kbd "C-g")
    (evil-lispy--bind
     (deactivate-mark)
     (evil-normal-state)))

  (define-key map [escape] (kbd "C-g"))

  (define-key map "i" #'evil-lispy-insert)
  (define-key map "A" #'lispy-beginning-of-defun)
  (evil-lispy-define-key map "RET" #'lispy-newline-and-indent :hybrid)
  (define-key map "o" #'evil-lispy-open-above)
  (define-key map "O" #'evil-lispy-open-below)

  ;; Navigation
  (evil-lispy-define-key map "(" #'lispy-parens :hybrid)
  (define-key map (kbd ")") #'lispy-out-forward)
  (define-key map "h" #'lispy-backward)
  (define-key map "j" #'lispy-down)
  (define-key map "k" #'lispy-up)
  (define-key map "l" #'lispy-forward)
  (define-key map "f" #'lispy-flow)
  (define-key map "%" #'lispy-different)
  (define-key map "a" #'evil-lispy-append)
  (define-key map "gd" #'lispy-follow)
  (define-key map "G" #'lispy-goto)
  (define-key map "q" #'lispy-ace-paren)
  (define-key map "Q" (lambda () (interactive) (lispy-ace-paren 2)))
  ;; FIXME: real func

  ;; Paredit transformations
  (define-key map ">" #'evil-lispy-alter-sexp-right)
  (define-key map "<" #'evil-lispy-alter-sexp-left)
  (define-key map "/" #'lispy-splice)
  (define-key map "r" #'lispy-raise)
  (define-key map "R" #'lispy-raise-some)
  (define-key map (kbd "gJ") #'lispy-split)
  ;; (define-key map "J" #'lispy-join)
  (define-key map "J" #'lispy-oneline)
  (define-key map "C" #'lispy-convolute)
  (define-key map (kbd "C-k") #'lispy-move-up)
  (define-key map (kbd "C-j") #'lispy-move-down)
  ;; (define-key map "O" #'lispy-oneline)
  (define-key map "M" #'lispy-multiline)
  (evil-lispy-define-key map ";" #'lispy-comment :hybrid)
  (define-key map "C" #'lispy-clone)
  (define-key map "t" #'lispy-teleport)

  ;; Kill related
  (evil-lispy-define-key map "DEL" #'lispy-delete-backward :hybrid)
  (evil-lispy-define-key map "d" #'lispy-delete :jump)
  (evil-lispy-define-key map "c" #'lispy-delete :insert)
  (evil-lispy-define-key map "p" #'lispy-paste :jump)
  (define-key map "P" #'yank-pop)
  (define-key map "n" #'evil-lispy-enter-transient-normal-state)
  (define-key map "y" #'lispy-new-copy)


  ;; Marking
  (define-key map "s" #'lispy-ace-symbol)
  (evil-lispy-define-key map "gs" #'lispy-ace-symbol-replace :insert)
  (define-key map "v" #'lispy-mark-symbol)
  (define-key map "V" #'lispy-mark-list)


  ;; Misc
  (define-key map "u" #'lispy-undo)
  (define-key map "e" #'lispy-eval)
  (define-key map "E" #'lispy-eval-and-insert)
  (define-key map "K" #'lispy-describe)
  (define-key map "F" #'lispy-follow)
  (define-key map "." #'evil-lispy-repeat)
  ;; (define-key map "" #'lispy-describe)

  (define-key map "gq" #'lispy-normalize)
  (define-key map "=" #'lispy-tab)
  (define-key map (kbd "TAB") #'lispy-shifttab)
  (define-key map "zz" #'evil-scroll-line-to-center)

  ;; Outline
  (define-key map "gj" #'lispy-outline-next)
  (define-key map "gk" #'lispy-outline-prev)
  ;; Digit argument
  (mapc (lambda (x) (define-key map (format "%d" x) #'digit-argument))
        (number-sequence 0 9)))

(provide 'evil-lispy)
;;; evil-lispy.el ends here
