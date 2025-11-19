;;; lispy-meow.el --- A lispy layer for meow -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Vince Vice
;;
;; Author: Vince Vice <vincent.troetschel@mailbox.org>
;; Maintainer: Vince Vice <vincent.troetschel@mailbox.org>
;; Created: November 18, 2025
;; Modified: November 18, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/fatal: not a git repository: /Users/indiana/.config/doom/lisp/../../.git/modules/engrave-faces/lispy-meow
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A lispy layer for meow.
;;
;;; Code:

(require 'meow)
(require 'lispy)

(defgroup lispy-meow nil
  "Integrate lispy and meow."
  :group 'external)

(defcustom lispy-meow-open-cmds-prefer-lispy-p t
  "If non-nil enter LISPY in newline commands instead of INSERT.
Affected are `meow-open-above' and `meow-open-below'."
  :type 'boolean
  :group 'lispy-meow)

(defcustom lispy-meow-beacon-prefer-lispy-p nil
  "If non-nil enter LISPY from BEACON instead of INSERT.
The resulting cursors may be unpredictable."
  :type 'boolean
  :group 'lispy-meow)

(defcustom lispy-meow-preserve-selection-on-entry-p t
  "If non-nil the selection will stay active when entering LISPY."
  :type 'boolean
  :group 'lispy-meow)


;;; Activation Mode

(defsubst lispy-meow-cmd-name (meow-insert-fn)
  "Return the name for the LISPY pendant to MEOW-INSERT-FN as a string.
I.e. (lispy-meow-cmd-name \\='meow-insert) => lispy-meow-insert"
  (concat "lispy-" (symbol-name meow-insert-fn)))

(defun lispy-meow-activate-p ()
  "Whether or not LISPY should replace INSERT."
  (and (bound-and-true-p lispy-meow-mode)
       (or lispy-meow-beacon-prefer-lispy-p
           (not (memq this-command '(meow-beacon-insert
                                     meow-beacon-append
                                     meow-beacon-change))))
       (or lispy-meow-open-cmds-prefer-lispy-p
           (not (memq this-command '(meow-open-above
                                     meow-open-below))))))

(defmacro lispy-meow-make-advice-maybe-lispy (meow-insert-fn)
  "Return advice function to advice MEOW-INSERT-FN with."
  (let ((adv-fn (intern (concat (symbol-name meow-insert-fn) "-maybe-lispy-a"))))
    `(defun ,adv-fn (insert-fn &rest r)
       ,(concat "Advice around `" (symbol-name meow-insert-fn) "'.\n"
                "Replace call to `" (symbol-name meow-insert-fn)
                "' with `" (lispy-meow-cmd-name meow-insert-fn) "'")
       (if (lispy-meow-activate-p)
           ,(cond ((eq meow-insert-fn 'meow-insert)
                   '(apply #'lispy-meow-insert r))
                  ((eq meow-insert-fn 'meow-append)
                   '(apply #'lispy-meow-append r))
                  ((eq meow-insert-fn 'meow-change)
                   '(apply #'lispy-meow-change r))
                  ((eq meow-insert-fn 'meow-open-above)
                   '(apply #'lispy-meow-open-above r))
                  ((eq meow-insert-fn 'meow-open-below)
                   '(apply #'lispy-meow-open-below r))
                  (t (apply meow-insert-fn r)))
         (apply insert-fn r)))))

;; Store the symbols so we can easily remove the advice in the teardown
(defvar lispy-meow--insert-a (lispy-meow-make-advice-maybe-lispy meow-insert))
(defvar lispy-meow--append-a (lispy-meow-make-advice-maybe-lispy meow-append))
(defvar lispy-meow--change-a (lispy-meow-make-advice-maybe-lispy meow-change))
(defvar lispy-meow--open-above-a (lispy-meow-make-advice-maybe-lispy meow-open-above))
(defvar lispy-meow--open-below-a (lispy-meow-make-advice-maybe-lispy meow-open-below))

;;;###autoload
(define-minor-mode lispy-meow-mode
  "Integrate `lispy-mode' into meow."
  :group 'lispy-meow
  :init-value nil
  (if lispy-meow-mode
      (progn (advice-add 'meow-insert :around lispy-meow--insert-a)
             (advice-add 'meow-append :around lispy-meow--append-a)
             (advice-add 'meow-change :around lispy-meow--change-a)
             (advice-add 'meow-open-above :around lispy-meow--open-above-a)
             (advice-add 'meow-open-below :around lispy-meow--open-below-a))
    (advice-remove 'meow-insert lispy-meow--insert-a)
    (advice-remove 'meow-append lispy-meow--append-a)
    (advice-remove 'meow-change lispy-meow--change-a)
    (advice-remove 'meow-open-above lispy-meow--open-above-a)
    (advice-remove 'meow-open-below lispy-meow--open-below-a)))


;;; Entrypoints
;;
;; Mirror meow-insert / meow-append / meow-change / ... with 2 differences
;; - Switch to LISPY instead of INSERT
;; - Don't cancel selection

;;;###autoload
(defun lispy-meow-insert ()
  "Move to the start of selection, switch to LISPY state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--direction-backward)
    (unless lispy-meow-preserve-selection-on-entry-p
      (meow--cancel-selection))
    (meow--switch-state 'lispy)
    (when meow-select-on-insert
      (setq-local meow--insert-pos (point)))))

;;;###autoload
(defun lispy-meow-append ()
  "Move to the start of selection, switch to LISPY state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (if (not (region-active-p))
        (when (and meow-use-cursor-position-hack
                   (< (point) (point-max)))
          (forward-char 1))
      (meow--direction-forward)
      (unless lispy-meow-preserve-selection-on-entry-p
        (meow--cancel-selection)))
    (meow--switch-state 'lispy)
    (when meow-select-on-append
      (setq-local meow--insert-pos (point)))))

;;;###autoload
(defun lispy-meow-change ()
  "Kill current selection and switch to LISPY state.

This command supports `meow-selection-command-fallback'."
  (interactive)
  (when (meow--allow-modify-p)
    (setq this-command #'lispy-meow-change)
    (meow--with-selection-fallback
     (meow--delete-region (region-beginning) (region-end))
     (meow--switch-state 'lispy)
     (when meow-select-on-change
       (setq-local meow--insert-pos (point))))))

(defun lispy-meow-change-char ()
  "Delete current char and switch to LISPY state."
  (interactive)
  (when (< (point) (point-max))
    (meow--execute-kbd-macro meow--kbd-delete-char)
    (meow--switch-state 'lispy)
    (when meow-select-on-change
      (setq-local meow--insert-pos (point)))))

(add-to-list 'meow-selection-command-fallback '(lispy-meow-change . lispy-meow-change-char))

;;;###autoload
(defun lispy-meow-open-above ()
  "Open a newline above and switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'lispy)
    (goto-char (line-beginning-position))
    (save-mark-and-excursion
      (newline))
    (indent-according-to-mode)))

;;;###autoload
(defun lispy-meow-open-below ()
  "Open a newline below and switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'lispy)
    (goto-char (line-end-position))
    (meow--execute-kbd-macro "RET")))


;;; Lispy State

;; The name is no accident, it it created by `meow-define-state'.
(defvar-keymap meow-lispy-state-keymap
  :doc "Keymap for Meow's Lispy state. This is custom code."
  :parent meow-insert-state-keymap)

(meow-define-state lispy
  "Meow LISPY state minor mode."
  :lighter " [L]"
  :keymap meow-lispy-state-keymap
  :face meow-insert-cursor
  (if meow-lispy-mode
      (progn (run-hooks 'meow-insert-enter-hook)
             (lispy-mode 1))
    (when (and meow--insert-pos
               (or meow-select-on-change
                   meow-select-on-append
                   meow-select-on-insert)
               (not (= (point) meow--insert-pos)))
      (thread-first
        (meow--make-selection '(select . transient) meow--insert-pos (point))
        (meow--select)))
    (run-hooks 'meow-insert-exit-hook)
    (lispy-mode -1)
    (setq-local meow--insert-pos nil
                ;; meow--insert-activate-mark nil
                )))

;; Teach meow-insert-exit to exit out of LISPY
;; The function does not test for the above defined lispy-meow-mode
;; (note the order of "lispy" and "meow")
;; but instead is defined by the meow-define-state macro.
(advice-add 'meow-insert-mode-p
            :after-until #'meow-lispy-mode-p)


;;; State Indicator

(defface lispy-meow-indicator
  '((((class color) (background dark))
     ())
    (((class color) (background light))
     ()))
  "Lispy state indicator."
  :group 'meow)

(add-to-list 'meow-indicator-face-alist `(lispy . lispy-meow-indicator))

(add-to-list 'meow-replace-state-name-list '(lispy . "LISPY"))


;;; Commands

;; Region aware kill (that still respects `lispy-safe-delete')

(defun lispy-meow--maybe-safe-kill-region (beg end)
  "Kill the region from BEG to END.
If `lispy-safe-delete' is non-nil, exclude unmatched delimiters.
Like `lispy--maybe-safe-delete-region' but modifies kill ring."
  (if lispy-safe-delete
      (let ((safe-regions (lispy--find-safe-regions beg end)))
        (dolist (safe-region safe-regions)
          (kill-region (car safe-region) (cdr safe-region))))
    (kill-region beg end)))

(defun lispy-meow-kill-dwim ()
  "Kill region or line, keeping parens consistent."
  (interactive)
  (let (bnd)
    (cond
     ;; In commment kill line
     ((or (lispy--in-comment-p)
          (and (looking-at " *;")
               (save-excursion
                 (goto-char (match-end 0))
                 (lispy--in-comment-p))))
      (kill-line))

     ;; In active region kill region
     ((region-active-p)
      (lispy-meow--maybe-safe-kill-region
       (region-beginning) (region-end)))

     ;; In string kill until eos or eol
     ((and (setq bnd (lispy--bounds-string))
           (or
            (not (eq (point) (car bnd)))
            (> (count-lines (car bnd) (cdr bnd)) 1)))
      (if (> (cdr bnd) (line-end-position))
          (if (eq (point) (car bnd))
              (kill-region (car bnd) (cdr bnd))
            (kill-line))
        (kill-region (point) (1- (cdr bnd)))))
     ((looking-at " *\n")
      (kill-region
       (match-beginning 0)
       (match-end 0))
      (lispy--indent-for-tab))
     ((and (looking-at lispy-right) (looking-back lispy-left
                                                  (line-beginning-position)))
      (delete-char 1)
      (delete-char -1))
     ((and (lispy-left-p)
           (if (memq major-mode lispy-elisp-modes)
               (not (eq (char-after) ?\{))
             t))
      (if (progn
            (setq bnd (lispy--bounds-list))
            (> (count-lines (car bnd) (cdr bnd)) 1))
          (kill-region (car bnd)
                       (cdr bnd))
        (narrow-to-region (car bnd) (line-end-position))
        (let ((pt (point)))
          (while (and (ignore-errors
                        (forward-list))
                      (> (point) pt))
            (setq pt (point)))
          (when (looking-at "[\t ]*;[^\n]*$")
            (setq pt (match-end 0)))
          (goto-char (point-min))
          (widen)
          (kill-region (point) pt))))
     (t
      (let ((beg (point))
            (end (line-end-position))
            bnd)
        (while (and (< (point) end)
                    (ignore-errors
                      (forward-sexp 1)
                      (skip-chars-forward " ,")
                      t))
          (when (setq bnd (lispy--bounds-comment))
            (goto-char (cdr bnd))))
        (skip-chars-forward " \t")
        (kill-region beg (point)))))))


;;; Cursor

(defcustom lispy-meow-cursor-type-default meow-cursor-type-insert
  "Default cursor type in LISPY state, when lispy state is not special."
  :type '(choice
          (choice (hollow bar box))
          (cons (const bar) natnum))
  :group 'lispy-meow)

(defcustom lispy-meow-cursor-type-special 'hollow
  "Cursor type in LISPY state when region is special."
  :type '(choice
          (choice (hollow bar box))
          (cons (const bar) natnum))
  :group 'lispy-meow)

;; Defined by state
(setq meow-cursor-type-lispy lispy-meow-cursor-type-default)

(defun lispy-meow--special-state-p ()
  "Return t if lispy is in special state.
The condition is from `lispy--insert-or-call'."
  (or (lispy-left-p)
      (lispy-right-p)
      (and (lispy-bolp)
           (or (looking-at lispy-outline-header)
               (looking-at lispy-outline)))))

(defvar-local lispy-meow--dynamic-cursor-last-pt nil
  "Caches last known point in the current (possibly indirect) buffer.
See `(elisp) Indirect Buffers'.
This allows running `lispy-meow--cursor-update' only when needed.")

(defun lispy-meow--cursor-update ()
  "Update the cursor based on whether lispy is in special state."
  (unless (eq (point) lispy-meow--dynamic-cursor-last-pt)
    (setq lispy-meow--dynamic-cursor-last-pt (point)
          meow-cursor-type-lispy
          (if (lispy-meow--special-state-p)
              lispy-meow-cursor-type-special
            lispy-meow-cursor-type-default))
    (meow--update-cursor)))

(define-minor-mode lispy-meow--dynamic-cursor-mode
  "A minor mode that changes the cursor in lispy's special state."
  :global nil
  (if lispy-meow--dynamic-cursor-mode
      ;; TODO can we use a less performance sensitive hook here only run after point changes?
      ;; In that case the above caching of point would not be necessary anymore.
      (add-hook 'post-command-hook #'lispy-meow--cursor-update nil t)
    (remove-hook 'post-command-hook #'lispy-meow--cursor-update t)))

(add-hook 'lispy-mode-hook
          (defun lispy-meow--toggle-dynamic-cursor-mode ()
            "Toggle cursor updates for lispy states (default, special)"
            (lispy-meow--dynamic-cursor-mode 'toggle)))


(provide 'lispy-meow)
;;; lispy-meow.el ends here
