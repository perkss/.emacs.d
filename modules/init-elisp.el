;;;; Configuration for Emacs Lisp

;;; Bind M-. to find-function instead of find-tag
(define-key emacs-lisp-mode-map [(meta .)] 'find-function)


(defun exordium-page-break-lines-hook ()
  "Enable `page-break-lines' mode.
When in TUI enable line truncation as well to prevent a rendering
bug (page break lines wrap around)."
  (unless (display-graphic-p)
    (set (make-local-variable 'truncate-lines) t))
  (page-break-lines-mode))

;;; Display page breaks with an horizontal line instead of ^L.
;;; Note: To insert a page break: C-q C-l
;;;       To jump to the previous/next page break: C-x [ and C-x ]
(use-package page-break-lines
  :diminish
  :hook
  (emacs-lisp-mode . exordium-page-break-lines-hook))

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

;;; Animation when evaluating a defun or a region:
(require 'facemenu)  ;; no longer preloaded in emacs 28, but used by `highlight' without require
(use-package highlight)
(use-package eval-sexp-fu)

(provide 'init-elisp)
