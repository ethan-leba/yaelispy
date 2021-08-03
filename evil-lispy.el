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

;;* Mode/State definition

(defvar-local evil-lispy-transient-normal-state nil)
;;;###autoload
(define-minor-mode evil-lispy-mode
  "Context sensitive paredit"
  :lighter " eLY"
  :keymap (make-sparse-keymap)
  :init-value nil
  :after-hook (evil-normal-state))

(defun evil-lispy-activate ()
  (setq evil-lispy-transient-normal-state nil))

(defun evil-lispy-deactivate ())

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

(defvar evil-lispy-normal-exit-commands
  '(lispyville-delete
    lispyville-delete-line
    lispyville-delete-char-or-splice
    evil-replace)
  "The list of Normal state commands that should trigger re-entry into Lispy state.")

(defun evil-lispy-execute-in-normal-state ()
  "Execute the next command in Normal state."
  (interactive)
  (evil-delay `(memq this-command `,evil-lispy-normal-exit-commands)
      `(progn
         (with-current-buffer ,(current-buffer)
           (message "Resetting state to %S" ',evil-state)
           (evil-lispy-state)
           (lispy-backward 1)))
    'post-command-hook)
  (evil-normal-state)
  (evil-echo "Switched to Normal state for the next command ..."))

(defmacro evil-lispy--bind (&rest code)
  "Helper to make an bindable command"
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
     (interactive) (call-interactively #',fun) (lispy-backward 1)))

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

(defvar evil-lispy--command-wrappers
  '((:insert . evil-lispy--insert-after)
   (:jump . evil-lispy--jump-back)
   (:hybrid . evil-lispy--insert-nonspecial)))


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

(defun evil-lispy-transient-normal ()
  "Move bound of sexp right"
  (interactive)
  (setq evil-lispy-transient-normal-state t)
  (deactivate-mark)
  (evil-normal-state))

(defun evil-lispy-insert ()
  "Move bound of sexp left"
  (interactive)
  (when (lispy-right-p)
    (backward-char))
  (evil-insert 0)
  (deactivate-mark))

(defun evil-lispy-append ()
  "Move bound of sexp left"
  (interactive)
  (when (lispy-right-p)
    (backward-char))
  (evil-append 0)
  (deactivate-mark))

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
  (evil-define-key 'insert map [escape]
    (evil-lispy--bind
     (evil-lispy-state)
     (lispy-backward 1))))


;;* Lispy state binds

(let ((map evil-lispy-state-map))
  ;; Exiting state
  (define-key map (kbd "C-g")
    (evil-lispy--bind
     (deactivate-mark)
     (evil-normal-state)))

  (define-key map [escape] (kbd "C-g"))

  (define-key map "i" #'evil-lispy-insert)
  (define-key map "a" #'evil-lispy-append)
  (define-key map "A" #'lispy-beginning-of-defun)
  (evil-lispy-define-key map "RET" #'lispy-newline-and-indent :hybrid)

  ;; Navigation
  ;; (define-key map (kbd "(") (evil-lispy--insert-after (lispy-parens 0)))
  (define-key map (kbd ")") #'lispy-out-forward)
  (define-key map "h" #'lispy-backward)
  (define-key map "j" #'lispy-down)
  (define-key map "k" #'lispy-up)
  (define-key map "l" #'lispy-forward)
  (define-key map "f" #'lispy-flow)
  (define-key map "o" #'lispy-different)
  (define-key map "gd" #'lispy-follow)
  (define-key map "G" #'lispy-goto)
  (define-key map "q" #'lispy-ace-paren)
  (define-key map "Q" #'lispy-ace-char)

  ;; Paredit transformations
  (define-key map ">" #'evil-lispy-alter-sexp-right)
  (define-key map "<" #'evil-lispy-alter-sexp-left)
  (define-key map "/" #'lispy-splice)
  (define-key map "r" #'lispy-raise)
  (define-key map "R" #'lispy-raise-some)
  (define-key map (kbd "gJ") #'lispy-split)
  (define-key map "J" #'lispy-join)
  (define-key map "C" #'lispy-convolute)
  (define-key map (kbd "C-k") #'lispy-move-up)
  (define-key map (kbd "C-j") #'lispy-move-down)
  (define-key map "O" #'lispy-oneline)
  (define-key map "M" #'lispy-multiline)
  (define-key map (kbd ";") #'lispy-comment)
  (define-key map "C" #'lispy-clone)
  (define-key map "t" #'lispy-teleport)

  ;; Kill related
  ;; (define-key map (kbd "DEL")
  ;;   (evil-lispy--jump-back (lispy-delete-backward 0)))
  (evil-lispy-define-key map "d" #'lispy-delete :jump)
  (evil-lispy-define-key map "c" #'lispy-delete :insert)
  (evil-lispy-define-key map "p" #'lispy-paste :jump)
  (define-key map "P" #'yank-pop)
  (define-key map "n" #'evil-lispy-execute-in-normal-state)
  (define-key map "y" #'lispy-new-copy)


  ;; Marking
  (define-key map "s" #'lispy-ace-symbol)
  (define-key map "gs" #'lispy-ace-symbol-replace)
  (define-key map "v" #'lispy-mark-symbol)
  (define-key map "V" #'lispy-mark-list)


  ;; Misc
  (define-key map "u" #'lispy-undo)
  (define-key map "e" #'lispy-eval)
  (define-key map "E" #'lispy-eval-and-insert)
  (define-key map "K" #'lispy-describe)

  (define-key map "gq" #'lispy-normalize)
  (define-key map "=" #'lispy-tab)
  (define-key map (kbd "TAB") #'lispy-shifttab)
  (define-key map "z" #'lispy-view)

  ;; Outline
  (define-key map "gj" #'lispy-outline-next)
  (define-key map "gk" #'lispy-outline-prev)
  ;; Digit argument
  (mapc (lambda (x) (define-key map (format "%d" x) #'digit-argument))
        (number-sequence 0 9)))
